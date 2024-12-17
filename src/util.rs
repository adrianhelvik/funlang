pub fn join(sep: &str, values: Vec<String>) -> String {
    if let Some(s) = values.get(0) {
        let mut result = s.to_string();
        for s in values.iter().skip(1) {
            result.push_str(sep);
            result.push_str(&s);
        }
        result
    } else {
        String::new()
    }
}

