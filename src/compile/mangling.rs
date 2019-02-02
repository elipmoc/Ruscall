use crate::compile::types::types::Type;


pub fn mangle(base_name: &String, ty: &Type) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    ty.hash(&mut hasher);
    base_name.clone() + "#" + &hasher.finish().to_string()
}
