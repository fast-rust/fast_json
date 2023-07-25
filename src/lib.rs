
//! See https://www.json.org/json-en.html
//!
//! 

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum State {
    Value,
    ArrayValue,
    ObjectKey,
    ObjectValue,
}

pub trait JsonConsumer<'v> {
    fn object_start(&mut self, state: State) -> Result<(), &'static str>;
    fn array_start(&mut self, state: State) -> Result<(), &'static str>;
    fn null(&mut self, state: State) -> Result<(), &'static str>;
    fn boolean(&mut self, state: State, is_true: bool) -> Result<(), &'static str>;
    fn number(&mut self, state: State, text: &'v [u8]) -> Result<(), &'static str>;
    fn string(&mut self, state: State, text: &'v [u8]) -> Result<(), &'static str>;
    fn object_end(&mut self) -> Result<(), &'static str>;
    fn array_end(&mut self) -> Result<(), &'static str>;
}

pub fn parse<'v, C : JsonConsumer<'v>>(consumer: &mut C, src: &'v str) -> Result<(), &'static str> {
    use State::*;
    let mut src = src.as_bytes();
    let mut state = Value;
    const INIT : State = Value;
    const STACK_SIZE : usize = 1024;
    let mut state_stack = [INIT; STACK_SIZE];
    let mut stack_ptr = 0;

    while !src.is_empty() {
        let b = src[0];
        if b.is_ascii_whitespace() {
            src = &src[1..];
            continue;
        }

        if b == b'"' {
            let index = src.windows(2)
                .position(|w| w[0] != b'\\' && w[1] == b'"' )
                .ok_or("Closing quote not found.")?;
            consumer.string(state, &src[1..index+1])?;
            src = &src[index+2..];
        } else if b.is_ascii_digit() ||  b == b'-' {
            let index = src.iter()
                .position(|&b| !(b.is_ascii_digit() || b == b'.' || b == b'-' || b == b'+' || (b & !0x20) == b'E'))
                .unwrap_or(src.len());
            consumer.number(state, &src[0..index])?;
            src = &src[index..];
        } else if b == b'[' || b == b'{' {
            match b {
                b'[' => consumer.array_start(state)?,
                b'{' => consumer.object_start(state)?,
                _=> return Err("mismatched closing character"),
            }
            if stack_ptr == STACK_SIZE {
                return Err("stack overflow");
            }
            state_stack[stack_ptr] = state;
            stack_ptr += 1;
            state = if b == b'[' { ArrayValue } else { ObjectKey };
            src = &src[1..];
            continue;
        } else if (b == b']' || b == b'}') && stack_ptr != 0 {
            match (state, b) {
                (ArrayValue, b']') => consumer.array_end()?,
                (ObjectKey, b'}') => consumer.object_end()?,
                _=> return Err("mismatched closing character"),
            }
            stack_ptr -= 1;
            state = state_stack[stack_ptr];
            src = &src[1..];
        } else if src.len() >= 4 && &src[0..4] == b"null" {
            consumer.null(state)?;
            src = &src[4..];
        } else if src.len() >= 4 && &src[0..4] == b"true" {
            consumer.boolean(state, true)?;
            src = &src[4..];
        } else if src.len() >= 5 && &src[0..5] == b"false" {
            consumer.boolean(state, false)?;
            src = &src[5..];
        } else {
            return Err("unexpected character");
        }

        while !src.is_empty() && src[0].is_ascii_whitespace() {
            src = &src[1..];
        }

        if state == ObjectKey && b != b'"' {
            return Err("unexpected string for key");
        }

        let (sep, close, new_state) = match state {
            ObjectKey => (b':', b'}', ObjectValue),
            ObjectValue => (b',', b'}', ObjectKey),
            ArrayValue => (b',', b']', ArrayValue),
            Value => break,
        };
        
        if src.is_empty() || (src[0] != sep && src[0] != close) {
            eprintln!("err {} {} {}", sep as char, close as char, std::str::from_utf8(src).unwrap());
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
    Object(Vec<(&'v str, Value<'v>)>),
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
    pub fn from_str(src: &'v str) -> Result<Value<'v>, &'static str> {
        let mut v = ValueBuilder::default();
        parse(&mut v, src)?;
        v.values.pop().ok_or("underflow")
    }
}

impl<'v> JsonConsumer<'v> for ValueBuilder<'v> {
    fn object_start(&mut self, _state: State) -> Result<(), &'static str> {
        self.starts.push(self.values.len());
        Ok(())
    }

    fn array_start(&mut self, _state: State) -> Result<(), &'static str> {
        self.starts.push(self.values.len());
        Ok(())
    }

    fn null(&mut self, _state: State) -> Result<(), &'static str> {
        self.values.push(Value::Null);
        Ok(())
    }

    fn boolean(&mut self, _state: State, is_true: bool) -> Result<(), &'static str> {
        self.values.push(Value::Boolean(is_true));
        Ok(())
    }

    fn number(&mut self, _state: State, text: &'v [u8]) -> Result<(), &'static str> {
        let s = std::str::from_utf8(text)
            .map_err(|_| "mon-UTF8 string")?;
        let n = s.parse().map_err(|_| "bad number")?;
        self.values.push(Value::Number(n));
        Ok(())
    }

    fn string(&mut self, _state: State, text: &'v [u8]) -> Result<(), &'static str> {
        let s = std::str::from_utf8(text)
            .map_err(|_| "mon-UTF8 string")?;
        self.values.push(Value::String(s));
        Ok(())
    }

    fn object_end(&mut self) -> Result<(), &'static str> {
        let start = self.starts.pop().ok_or("unbalanced end")?;
        let mut object = vec![];
        let mut d = self.values.drain(start..);
        while let Some(key) = d.next() {
            let Value::String(key) = key else { return Err("expected string") };
            let value = d.next().ok_or("expected value")?;
            object.push((key, value));
        }
        drop(d);
        self.values.push(Value::Object(object));
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
        fn object_start(&mut self, _state: crate::State) -> Result<(), &'static str> {
            self.comma();
            self.res.push_str("{");
            Ok(())
        }

        fn array_start(&mut self, _state: crate::State) -> Result<(), &'static str> {
            self.comma();
            self.res.push_str("[");
            Ok(())
        }

        fn null(&mut self, _state: crate::State) -> Result<(), &'static str> {
            self.comma();
            self.res.push_str("null");
            Ok(())
        }

        fn boolean(&mut self, _state: crate::State, is_true: bool) -> Result<(), &'static str> {
            self.comma();
            self.res.push_str(if is_true { "true" } else { "false"});
            Ok(())
        }

        fn number(&mut self, _state: crate::State, text: &[u8]) -> Result<(), &'static str> {
            self.comma();
            self.res.push_str(std::str::from_utf8(text).unwrap());
            Ok(())
        }

        fn string(&mut self, state: crate::State, text: &[u8]) -> Result<(), &'static str> {
            self.comma();
            // self.res.push_str(&format!("{:?}", std::str::from_utf8(text).unwrap()));
            self.res.push_str("\"");
            self.res.push_str(std::str::from_utf8(text).unwrap());
            self.res.push_str("\"");
            if state == crate::State::ObjectKey { self.res.push_str(":"); }
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
        assert_eq!(Value::from_str(r#"{"xyz":1,"xyz":1}"#)?, Object(vec![("xyz", Number(1.0)), ("xyz", Number(1.0))]));
        Ok(())
    }
}

