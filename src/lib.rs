
//! See https://www.json.org/json-en.html
//!
//! 

use std::mem::MaybeUninit;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum State {
    Value,
    ArrayValue,
    ObjectKey,
    ObjectValue,
}

pub trait JsonConsumer<'v> {
    fn object_start(&mut self) -> Result<(), &'static str>;
    fn array_start(&mut self) -> Result<(), &'static str>;
    fn null(&mut self) -> Result<(), &'static str>;
    fn boolean(&mut self, is_true: bool) -> Result<(), &'static str>;
    fn number(&mut self, text: &'v [u8]) -> Result<(), &'static str>;
    fn key(&mut self, text: &'v [u8]) -> Result<(), &'static str>;
    fn string(&mut self, text: &'v [u8]) -> Result<(), &'static str>;
    fn object_end(&mut self) -> Result<(), &'static str>;
    fn array_end(&mut self) -> Result<(), &'static str>;
}

#[repr(u8)]
#[derive(Debug, PartialEq)]
enum Starts {
    X,
    W,
    S,
    N,
    B,
    E,
    T,
    F,
    U,
}

pub fn parse<'v, C : JsonConsumer<'v>>(consumer: &mut C, src: &'v str) -> Result<(), &'static str> {
    use State::*;
    let mut src = src.as_bytes();
    let mut state = Value;
    const STACK_SIZE : usize = 1024;
    let mut state_stack: [std::mem::MaybeUninit<State>; STACK_SIZE] =
        unsafe { std::mem::MaybeUninit::uninit().assume_init() };
    let mut stack_ptr = 0;

    use Starts::*;
    const STARTS : [Starts; 256] = [
        //                        \t \n       \r
        X, X, X, X, X, X, X, X, X, W, W, X, X, W, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X,
        // !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?
        W, X, S, X, X, X, X, X, X, X, X, N, X, N, N, X, N, N, N, N, N, N, N, N, N, N, X, X, X, X, X, X,
        // A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _
        X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, B, X, E, X, X,
        // a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  p  q  r  s  t  u  v  w  x  y  z  {  |  }  ~
        X, X, X, X, X, X, F, X, X, X, X, X, X, X, U, X, X, X, X, X, T, X, X, X, X, X, X, B, X, E, X, X,
        // Non-ASCII
        X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X,
        X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X,
        X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X,
        X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X,
    ];
    
    macro_rules! true_false_null {
        ($s: expr, $f: expr) => {
            if src.len() >= $s.len() && &src[0..$s.len()] == $s {
                $f?;
                src = &src[$s.len()..];
            } else {
                return Err("Unexpected chararacter.")
            }
        }
    }
    
    while !src.is_empty() {
        let b = src[0];
        if false { eprintln!("b={b:02x} s={:?}", STARTS[b as usize]); }
        match STARTS[b as usize] {
            X => return Err("Unexpected character."),
            W => { src = &src[1..]; continue; }
            S => {
                let index = src.windows(2)
                    .position(|w| w[1] == b'"' && w[0] != b'\\')
                    .ok_or("Closing quote not found.")?;
                if state == ObjectKey {
                    consumer.key(&src[1..index+1])?;
                } else {
                    consumer.string(&src[1..index+1])?;
                }
                src = &src[index+2..];
            }
            N => {
                let index = src.iter()
                    .position(|&b| STARTS[b as usize] != N && (b & !0x20) != b'E')
                    .unwrap_or(src.len());
                consumer.number(&src[0..index])?;
                src = &src[index..];
            }
            B => {
                match b {
                    b'[' => consumer.array_start()?,
                    b'{' => consumer.object_start()?,
                    _=> return Err("Mismatched closing character."),
                }
                if stack_ptr == STACK_SIZE {
                    return Err("Stack overflow.");
                }
                state_stack[stack_ptr] = MaybeUninit::new(state);
                stack_ptr += 1;
                state = if b == b'[' { ArrayValue } else { ObjectKey };
                src = &src[1..];
                continue;
            }
            E => {
                if stack_ptr == 0 { return Err("Mismatched closing character.")}
                match (state, b) {
                    (ArrayValue, b']') => consumer.array_end()?,
                    (ObjectKey, b'}') => consumer.object_end()?,
                    _=> return Err("Mismatched closing character."),
                }
                stack_ptr -= 1;
                state = unsafe { state_stack[stack_ptr].assume_init() };
                src = &src[1..];
            }
            T => true_false_null!(b"true", consumer.boolean(true)),
            F => true_false_null!(b"false", consumer.boolean(false)),
            U => true_false_null!(b"null", consumer.null()),
        }

        while !src.is_empty() && src[0].is_ascii_whitespace() {
            src = &src[1..];
        }

        if state == ObjectKey && b != b'"' {
            return Err("unexpected string for key");
        }

        let (sep, close, new_state) = match state {
            ObjectKey => (b':', b':', ObjectValue),
            ObjectValue => (b',', b'}', ObjectKey),
            ArrayValue => (b',', b']', ArrayValue),
            Value => break,
        };
        
        if src.is_empty() || (src[0] != sep && src[0] != close) {
            if false { eprintln!("err {} {} {}", sep as char, close as char, std::str::from_utf8(src).unwrap()); }
            return Err("expected separator or close");
        }
        if src[0] == sep { src = &src[1..]; }
        state = new_state;
    }
    if !src.is_empty() {
        eprintln!("src={:?}", src);
        return Err("trailing characters");
    }

    Ok(())
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value<'v> {
    Object(Vec<Value<'v>>),
    Array(Vec<Value<'v>>),
    Number(f64),
    String(&'v str),
    Boolean(bool),
    Null,
}

#[derive(Default)]
pub struct ValueBuilder<'v> {
    starts: Vec<usize>,
    values: Vec<Value<'v>>,
}

impl<'v> Value<'v> {
    /// # Example
    /// 
    /// ```
    /// use fast_json::Value;
    /// 
    /// assert_eq!(Value::from_str("1").unwrap(), Value::Number(1.0));
    /// ```
    pub fn from_str(src: &'v str) -> Result<Value<'v>, &'static str> {
        let mut v = ValueBuilder::default();
        parse(&mut v, src)?;
        v.values.pop().ok_or("underflow")
    }
}

impl<'v> JsonConsumer<'v> for ValueBuilder<'v> {
    fn object_start(&mut self) -> Result<(), &'static str> {
        self.starts.push(self.values.len());
        Ok(())
    }

    fn array_start(&mut self) -> Result<(), &'static str> {
        self.starts.push(self.values.len());
        Ok(())
    }

    fn null(&mut self) -> Result<(), &'static str> {
        self.values.push(Value::Null);
        Ok(())
    }

    fn boolean(&mut self, is_true: bool) -> Result<(), &'static str> {
        self.values.push(Value::Boolean(is_true));
        Ok(())
    }

    fn number(&mut self, text: &'v [u8]) -> Result<(), &'static str> {
        let s = std::str::from_utf8(text)
            .map_err(|_| "mon-UTF8 string")?;
        let n = s.parse().map_err(|_| "bad number")?;
        self.values.push(Value::Number(n));
        Ok(())
    }

    fn key(&mut self, text: &'v [u8]) -> Result<(), &'static str> {
        let s = std::str::from_utf8(text)
            .map_err(|_| "mon-UTF8 string")?;
        self.values.push(Value::String(s));
        Ok(())
    }

    fn string(&mut self, text: &'v [u8]) -> Result<(), &'static str> {
        let s = std::str::from_utf8(text)
            .map_err(|_| "mon-UTF8 string")?;
        self.values.push(Value::String(s));
        Ok(())
    }

    fn object_end(&mut self) -> Result<(), &'static str> {
        let start = self.starts.pop().ok_or("unbalanced end")?;
        let d = self.values.drain(start..);
        let array = d.collect();
        self.values.push(Value::Object(array));
        Ok(())
    }

    fn array_end(&mut self) -> Result<(), &'static str> {
        let start = self.starts.pop().ok_or("unbalanced end")?;
        let d = self.values.drain(start..);
        let array = d.collect();
        self.values.push(Value::Array(array));
        Ok(())
    }
}


#[cfg(test)]
mod test {
    use crate::{JsonConsumer, parse, Value};

    #[derive(Default)]
    struct TestConsumer {
        res: String
    }

    impl TestConsumer {
        fn comma(&mut self) {
            match self.res.as_bytes().last() {
                None | Some(b'{') | Some(b':') | Some(b'[') => {
                }
                _ => {
                    self.res.push(',');
                }
            }
        }
    }

    impl<'v> JsonConsumer<'v> for TestConsumer {
        fn object_start(&mut self) -> Result<(), &'static str> {
            self.comma();
            self.res.push_str("{");
            Ok(())
        }

        fn array_start(&mut self) -> Result<(), &'static str> {
            self.comma();
            self.res.push_str("[");
            Ok(())
        }

        fn null(&mut self) -> Result<(), &'static str> {
            self.comma();
            self.res.push_str("null");
            Ok(())
        }

        fn boolean(&mut self, is_true: bool) -> Result<(), &'static str> {
            self.comma();
            self.res.push_str(if is_true { "true" } else { "false"});
            Ok(())
        }

        fn number(&mut self, text: &[u8]) -> Result<(), &'static str> {
            self.comma();
            self.res.push_str(std::str::from_utf8(text).unwrap());
            Ok(())
        }

        fn key(&mut self, text: &[u8]) -> Result<(), &'static str> {
            self.comma();
            self.res.push_str("\"");
            self.res.push_str(std::str::from_utf8(text).unwrap());
            self.res.push_str("\":");
            Ok(())
        }

        fn string(&mut self, text: &[u8]) -> Result<(), &'static str> {
            self.comma();
            self.res.push_str("\"");
            self.res.push_str(std::str::from_utf8(text).unwrap());
            self.res.push_str("\"");
            Ok(())
        }

        fn object_end(&mut self) -> Result<(), &'static str> {
            self.res.push_str("}");
            Ok(())
        }

        fn array_end(&mut self) -> Result<(), &'static str> {
            self.res.push_str("]");
            Ok(())
        }
    }

    macro_rules! round_trip {
        ($s : expr) => {
            let mut t = TestConsumer::default();
            parse(&mut t, $s).unwrap();
            assert_eq!(t.res, $s);
        }
    }

    #[test]
    fn test() {
        let mut t = TestConsumer::default();
        parse(&mut t, " 1").unwrap();
        assert_eq!(t.res, "1");

        let mut t = TestConsumer::default();
        parse(&mut t, "1 ").unwrap();
        assert_eq!(t.res, "1");

        round_trip!("1.0e+9");
        round_trip!(r#""xyz""#);
        round_trip!(r#""x\"yz""#);
        round_trip!(r#"null"#);
        round_trip!(r#"true"#);
        round_trip!(r#"false"#);

        round_trip!(r#"[]"#);
        round_trip!(r#"[1]"#);
        round_trip!(r#"[1,2,3]"#);
        round_trip!(r#"[[]]"#);
        round_trip!(r#"{}"#);
        round_trip!(r#"{"xyz":1}"#);
        round_trip!(r#"{"xyz":1,"xyz":1}"#);

    }

    #[test]
    fn test_value() -> Result<(), &'static str> {
        use Value::*;
        assert_eq!(Value::from_str("1")?, Number(1.0));
        assert_eq!(Value::from_str("-1e+9")?, Number(-1e+9));
        assert_eq!(Value::from_str(r#""xyz""#)?, String("xyz"));
        assert_eq!(Value::from_str(r#""x\"yz""#)?, String(r#"x\"yz"#));
        assert_eq!(Value::from_str("null")?, Null);
        assert_eq!(Value::from_str("true")?, Boolean(true));
        assert_eq!(Value::from_str("false")?, Boolean(false));
        assert_eq!(Value::from_str("[]")?, Array(vec![]));
        assert_eq!(Value::from_str("[1,[2],3]")?, Array(vec![Number(1.0), Array(vec![Number(2.0)]), Number(3.0)]));
        assert_eq!(Value::from_str(r#"{"xyz":1,"xyz":1}"#)?, Object(vec![String("xyz"), Number(1.0), String("xyz"), Number(1.0)]));
        Ok(())
    }
}

