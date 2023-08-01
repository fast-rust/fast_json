# fast_json

An example of a fast JSON parser.


# Example
```
use fast_json::Value;

assert_eq!(Value::from_str("1").unwrap(), Value::Number(1.0));
```
