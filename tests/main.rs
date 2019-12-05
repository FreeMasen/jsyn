use jsyn::*;
use resw::*;
use syn::parse_file;

#[test]
fn example() {
    let rust = "
struct Thing {
    stuff: u8,
    kind: String,
}";
    let f = parse_file(rust).unwrap();
    let mut w = Writer::new(std::fs::File::create("written.js").unwrap());
    for item in f.items {
        let ast = convert_item(&item).unwrap();
        w.write_part(&ast).unwrap();
    }
}