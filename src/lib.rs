use ressa::node;
use syn::{Expr, Item, ItemStatic, Lit, ItemConst};

pub fn convert_item(item: &Item) -> Option<node::ProgramPart> {
    match item {
        Item::Static(ref item) => Some(convert_static_item(item)),
        Item::Const(ref item) => Some(convert_const_item(item)),
        Item::Fn(ref item) => Some(node::ProgramPart::Decl(node::Declaration::Function(convert_fn(item)))),
        Item::Struct(ref item) => Some(node::ProgramPart::Decl(node::Declaration::Class(convert_struct_item(item)))),
        _ => None,
    }
}

fn convert_static_item(item: &ItemStatic) -> node::ProgramPart {
    let variable_kind = if let Some(_) = &item.mutability {
        node::VariableKind::Let
    } else {
        node::VariableKind::Const
    };
    let id = node::Pattern::Identifier(item.ident.to_string());
    let init = Some(convert_expr(&item.expr));
    node::ProgramPart::Decl(node::Declaration::Variable(
        variable_kind,
        vec![node::VariableDecl { id, init }],
    ))
}

fn convert_const_item(item: &ItemConst) -> node::ProgramPart {
    let variable_kind = node::VariableKind::Const;
    let id = node::Pattern::Identifier(item.ident.to_string());
    let init = Some(convert_expr(&item.expr));
    node::ProgramPart::Decl(node::Declaration::Variable(
        variable_kind,
        vec![node::VariableDecl { id, init }]
    ))
}

fn convert_fn(item: &syn::ItemFn) -> node::Function {
    let is_async = item.asyncness.is_some();
    let id = item.ident.to_string();
    let params: Vec<node::FunctionArg> = item.decl.inputs.iter().filter_map(|a| {
        match a {
            syn::FnArg::Captured(ref c) => Some(node::FunctionArg::Pattern(convert_pattern(&c.pat))),
            syn::FnArg::Inferred(ref p) => Some(node::FunctionArg::Pattern(convert_pattern(p))),
            _ => None,
        }
    }).collect();
    let body: Vec<node::ProgramPart> = item.block.stmts.iter().filter_map(|s| {
        match s {
            syn::Stmt::Expr(ref e) => Some(node::ProgramPart::Statement(node::Statement::Return(Some(convert_expr(e))))),
            syn::Stmt::Item(ref i) => convert_item(i),
            syn::Stmt::Local(ref l) => Some(node::ProgramPart::Decl(node::Declaration::Variable(node::VariableKind::Let, vec![
                    node::VariableDecl {
                        id: convert_pattern(&l.pats.iter().next().unwrap()),
                        init: if let Some((_, ref init)) = l.init {
                            Some(convert_expr(init))
                        } else {
                            None
                        },
                    }
                ]))),
            _ => None,
        }
    }).collect();
    node::Function {
        id: Some(id),
        generator: false,
        is_async,
        params,
        body,
    }
}

fn convert_struct_item(item: &syn::ItemStruct) -> node::Class {
    let id = item.ident.to_string();
    let body = convert_struct_fields(&item.fields);
    node::Class {
        id: Some(id),
        super_class: None,
        body: vec![body],
    }
}

fn convert_struct_fields(f: &syn::Fields) -> node::Property {
    let key = node::PropertyKey::Expr(node::Expression::ident("constructor"));
    let value: node::Function = match f {
        syn::Fields::Named(ref named) => {
            let idents: Vec<node::Expression> = named.named.iter().map(|f| node::Expression::Ident(f.ident.clone().unwrap().to_string())).collect();
            node::Function {
                id: None,
                is_async: false,
                generator: false,
                params: idents.iter().map(|i| node::FunctionArg::Expr(i.clone())).collect(),
                body: idents.iter().map(|e| {
                    node::ProgramPart::Statement(
                        node::Statement::Expr(
                            node::Expression::Assignment(
                                node::AssignmentExpression {
                                    left: node::AssignmentLeft::Expr(
                                        Box::new(node::Expression::member(node::Expression::ident("this"), e.clone(), false))
                                    ),
                                    operator: node::AssignmentOperator::Equal,
                                    right: Box::new(e.clone())
                                }
                            )
                        )
                    )
                }).collect()
            }        
        },
        syn::Fields::Unnamed(ref unnamed) => {
            let idxs: Vec<usize> = unnamed.unnamed.iter().enumerate().map(|(i, _)| i).collect();
            node::Function {
                id: None,
                is_async: false,
                generator: false,
                params: idxs.iter().map(|i| node::FunctionArg::Expr(node::Expression::Ident(format!("_{}", i)))).collect(),
                body: idxs.iter().map(|i| node::ProgramPart::Statement(
                    node::Statement::Expr(
                        node::Expression::Assignment(
                            node::AssignmentExpression {
                                left: node::AssignmentLeft::Expr(
                                    Box::new(node::Expression::member(node::Expression::ident("this"), node::Expression::Literal(node::Literal::Number(i.to_string())), true)),
                                ),
                                operator: node::AssignmentOperator::Equal,
                                right: Box::new(node::Expression::Ident(format!("_{}", i)))
                            }
                        )
                    )
                )).collect()
            }
        },
        syn::Fields::Unit => node::Function {
            id: None,
            is_async: false,
            generator: false,
            params: vec![],
            body: vec![],
        },
    };
    
    node::Property {
        key,
        computed: false,
        kind: node::PropertyKind::Ctor,
        method: true,
        short_hand: false,
        value: node::PropertyValue::Expr(node::Expression::Function(value)),
    }
}

fn convert_expr(expr: &Expr) -> node::Expression {
    match expr {
        Expr::Array(ref array) => node::Expression::Array(
            array
                .elems
                .iter()
                .map(convert_expr)
                .map(|e| Some(e))
                .collect(),
        ),
        Expr::Lit(ref lit) => convert_literal(&lit.lit),
        Expr::Call(ref call) => convert_call(call),
        Expr::MethodCall(ref call) => convert_method_call(call),
        Expr::Tuple(ref tup) => convert_tuple(tup),
        Expr::Binary(ref bin) => convert_binary(bin),
        Expr::Unary(ref un) => convert_unary(un),
        Expr::Cast(ref cast) => convert_expr(&cast.expr),
        Expr::Type(ref ty) => convert_expr(&ty.expr),
        Expr::Let(ref assign) => convert_let_expr(&assign),
        Expr::Path(ref p) => {
            let mut exprs: Vec<node::Expression> = p.path.segments.iter().map(|p| { node::Expression::Ident(p.ident.to_string())}).collect();
            if exprs.len() == 1 {
                exprs.pop().unwrap()
            } else {
                let mut items = exprs.into_iter();
                let mut ret = items.next().unwrap();
                for e in items {
                    ret = node::Expression::member(ret, e, false);
                }
                ret
            }
        },
        Expr::If(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::While(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::ForLoop(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Loop(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Match(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Closure(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Unsafe(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Block(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Assign(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::AssignOp(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Field(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Index(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Range(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Reference(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Break(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Continue(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Return(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Macro(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Struct(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Repeat(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Paren(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Group(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Try(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Async(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::TryBlock(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Yield(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Verbatim(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::Box(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
        Expr::InPlace(ref e) => unimplemented!("Unimplemented Expression: {:#?}", e),
    }
}

fn convert_call(call: &syn::ExprCall) -> node::Expression {
    let callee = convert_expr(&call.func);
    let args = call.args.iter().map(convert_expr).collect();
    node::Expression::Call(node::CallExpression::new(callee, args))
}

fn convert_method_call(call: &syn::ExprMethodCall) -> node::Expression {
    let object = convert_expr(&call.receiver);
    let property = node::Expression::Ident(call.method.to_string());
    let args = call.args.iter().map(convert_expr).collect();
    let callee = node::Expression::Member(node::MemberExpression::new(object, property, false));
    node::Expression::Call(node::CallExpression::new(callee, args))
}

fn convert_tuple(tup: &syn::ExprTuple) -> node::Expression {
    let elements = tup.elems
                .iter()
                .map(convert_expr)
                .map(|e| Some(e))
                .collect();
    node::Expression::Array(elements)
}

fn convert_literal(literal: &Lit) -> node::Expression {
    match literal {
        Lit::Str(ref s) => node::Expression::Literal(node::Literal::String(format!("{}", s.value()))),
        Lit::Byte(ref b) => node::Expression::Literal(node::Literal::Number(format!("{}", b.value()))),
        Lit::ByteStr(ref bs) => node::Expression::Array(bs.value().iter().map(|b| Some(node::Expression::Literal(node::Literal::Number(format!("{}", b))))).collect()),
        Lit::Char(ref c) => node::Expression::Literal(node::Literal::String(format!("{}", c.value()))),
        Lit::Int(ref int) => node::Expression::Literal(node::Literal::Number(format!("{}", int.value()))),
        Lit::Float(ref f) => node::Expression::Literal(node::Literal::Number(format!("{}", f.value()))),
        Lit::Bool(ref b) => node::Expression::Literal(node::Literal::Boolean(b.value)),
        Lit::Verbatim(ref l) => node::Expression::Literal(node::Literal::Number(format!("{}", l.token.to_string()))),
    }
}

#[derive(Clone, Debug)]
enum BinOp {
    Binary(node::BinaryOperator),
    Logical(node::LogicalOperator),
    Assignment(node::AssignmentOperator),
}

fn convert_binary(binary: &syn::ExprBinary) -> node::Expression {
    let left = convert_expr(&binary.left);
    let op = convert_operator(&binary.op);
    let right = convert_expr(&binary.right);
    match op {
        BinOp::Binary(op) => node::Expression::Binary(node::BinaryExpression::new(left, op, right)),
        BinOp::Logical(op) => node::Expression::Logical(node::LogicalExpression::new(op, left, right)),
        BinOp::Assignment(op) => node::Expression::Assignment(node::AssignmentExpression::new(op, node::AssignmentLeft::expr(left), right)),
    }
}

fn convert_unary(op: &syn::ExprUnary) -> node::Expression {
    match &op.op {
        syn::UnOp::Not(_) => {
            let expr = convert_expr(&op.expr);
            node::Expression::Unary(node::UnaryExpression::new(node::UnaryOperator::Not, true, expr))
        },
        syn::UnOp::Neg(_) => {
            let expr = convert_expr(&op.expr);
            node::Expression::Unary(node::UnaryExpression::new(node::UnaryOperator::Minus, true, expr))
        },
        _ => convert_expr(&op.expr)
    }
}

fn convert_operator(op: &syn::BinOp) -> BinOp {
    match op {
        syn::BinOp::Add(_) => BinOp::Binary(node::BinaryOperator::Plus),
        syn::BinOp::Sub(_) => BinOp::Binary(node::BinaryOperator::Minus),
        syn::BinOp::Mul(_) => BinOp::Binary(node::BinaryOperator::Times),
        syn::BinOp::Div(_) => BinOp::Binary(node::BinaryOperator::Over),
        syn::BinOp::Rem(_) => BinOp::Binary(node::BinaryOperator::Mod),
        syn::BinOp::And(_) => BinOp::Logical(node::LogicalOperator::And),
        syn::BinOp::Or(_) => BinOp::Logical(node::LogicalOperator::Or),
        syn::BinOp::BitXor(_) => BinOp::Binary(node::BinaryOperator::XOr),
        syn::BinOp::BitAnd(_) => BinOp::Binary(node::BinaryOperator::And),
        syn::BinOp::BitOr(_) => BinOp::Binary(node::BinaryOperator::Or),
        syn::BinOp::Shl(_) => BinOp::Binary(node::BinaryOperator::LeftShift),
        syn::BinOp::Shr(_) => BinOp::Binary(node::BinaryOperator::RightShift),
        syn::BinOp::Eq(_) => BinOp::Binary(node::BinaryOperator::Equal),
        syn::BinOp::Lt(_) => BinOp::Binary(node::BinaryOperator::LessThan),
        syn::BinOp::Le(_) => BinOp::Binary(node::BinaryOperator::LessThanEqual),
        syn::BinOp::Ne(_) => BinOp::Binary(node::BinaryOperator::NotEqual),
        syn::BinOp::Ge(_) => BinOp::Binary(node::BinaryOperator::GreaterThanEqual),
        syn::BinOp::Gt(_) => BinOp::Binary(node::BinaryOperator::GreaterThan),
        syn::BinOp::AddEq(_) => BinOp::Assignment(node::AssignmentOperator::PlusEqual),
        syn::BinOp::SubEq(_) => BinOp::Assignment(node::AssignmentOperator::MinusEqual),
        syn::BinOp::MulEq(_) => BinOp::Assignment(node::AssignmentOperator::TimesEqual),
        syn::BinOp::DivEq(_) => BinOp::Assignment(node::AssignmentOperator::DivEqual),
        syn::BinOp::RemEq(_) => BinOp::Assignment(node::AssignmentOperator::ModEqual),
        syn::BinOp::BitXorEq(_) => BinOp::Assignment(node::AssignmentOperator::XOrEqual),
        syn::BinOp::BitAndEq(_) => BinOp::Assignment(node::AssignmentOperator::AndEqual),
        syn::BinOp::BitOrEq(_) => BinOp::Assignment(node::AssignmentOperator::OrEqual),
        syn::BinOp::ShlEq(_) => BinOp::Assignment(node::AssignmentOperator::LeftShiftEqual),
        syn::BinOp::ShrEq(_) => BinOp::Assignment(node::AssignmentOperator::RightShiftEqual),
    }
}

fn convert_let_expr(expr: &syn::ExprLet) -> node::Expression {
    let _kind = node::VariableKind::Let;
    let _idents: Vec<node::Pattern> = expr.pats.iter().map(convert_pattern).collect();
    unimplemented!("convert_let_expr {:#?}", expr);
}

fn convert_pattern(pat: &syn::Pat) -> node::Pattern {
    match pat {
        syn::Pat::Wild(_) => node::Pattern::Identifier("_".to_string()),
        syn::Pat::Ident(ref i) => node::Pattern::Identifier(i.ident.to_string()),
        syn::Pat::Struct(ref s) => {
            let parts = s.fields.iter().map(|f| node::ObjectPatternPart::Assignment(node::Property {
                key: node::PropertyKey::Pattern(convert_pattern(&f.pat)),
                value: node::PropertyValue::None,
                method: false,
                kind: node::PropertyKind::Init,
                short_hand: true,
                computed: false,
            })).collect();
            node::Pattern::Object(parts)
        },
        syn::Pat::Tuple(ref tup) => {
            let parts = tup.front.iter().chain(tup.back.iter()).map(pat_to_array_part).collect();
            node::Pattern::Array(parts)
        },
        syn::Pat::Box(ref expr) => convert_pattern(&expr.pat),
        syn::Pat::Ref(ref r) => convert_pattern(&r.pat),
        syn::Pat::Slice(ref s) => {
            let mut parts: Vec<Option<node::ArrayPatternPart>> = s.front.iter().map(pat_to_array_part).collect();
            if let Some(ref m) = s.middle {
                parts.push(pat_to_array_part(m));
            } 
            parts.append(&mut s.back.iter().map(pat_to_array_part).collect());
            node::Pattern::Array(parts)
        },
        _ => unimplemented!("Unimplemented pattern {:#?}", pat),
    }
}

fn pat_to_array_part(pat: &syn::Pat) -> Option<node::ArrayPatternPart> {
    Some(node::ArrayPatternPart::Patt(convert_pattern(pat)))
} 

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_file;
    #[test]
    fn static_array() {
        let mut rust = "static X: [u8;3] = [0,1,2];";
        let parsed = parse_file(&mut rust).unwrap();
        for item in parsed.items {
            let part = convert_item(&item).unwrap();
            assert_eq!(
                part,
                node::ProgramPart::Decl(node::Declaration::Variable(
                    node::VariableKind::Const,
                    vec![node::VariableDecl::with_value(
                        "X",
                        node::Expression::Array(vec![
                            Some(node::Expression::number("0")),
                            Some(node::Expression::number("1")),
                            Some(node::Expression::number("2")),
                        ])
                    )]
                ))
            )
        }
    }
    #[test]
    fn static_mut_array() {
        let mut rust = "static mut X: [u8;3] = [0,1,2];";
        let parsed = parse_file(&mut rust).unwrap();
        for item in parsed.items {
            let part = convert_item(&item).unwrap();
            assert_eq!(
                part,
                node::ProgramPart::Decl(node::Declaration::Variable(
                    node::VariableKind::Let,
                    vec![node::VariableDecl::with_value(
                        "X",
                        node::Expression::Array(vec![
                            Some(node::Expression::number("0")),
                            Some(node::Expression::number("1")),
                            Some(node::Expression::number("2")),
                        ])
                    )]
                ))
            )
        }
    }
    #[test]
    fn const_int() {
        let mut rust = "const X: u8 = 0;";
        let parsed = parse_file(&mut rust).unwrap();
        for item in parsed.items {
            let part = convert_item(&item).unwrap();
            assert_eq!(
                part,
                node::ProgramPart::Decl(node::Declaration::Variable(
                    node::VariableKind::Const,
                    vec![
                        node::VariableDecl::with_value("X", node::Expression::number("0"))
                    ]
                ))
            )
        }
    }

    #[test]
    fn pattern_obj() {
        let mut rust = r#"
fn main() {
    let Foo { x, y, .. } = Foo { x: 0, y: 0, z: 0 };
}"#;
        let parsed = parse_file(&mut rust).unwrap();
        for item in parsed.items {
            println!("{:#?}", item);
        }
    }
    #[test]
    fn tup_obj() {
        let mut rust = r#"
fn main() {
    let (first, second, third) = (8, 8, 8);
}"#;
        let parsed = parse_file(&mut rust).unwrap();
        let item = parsed.items.iter().next().unwrap();
        let out = convert_item(&item).unwrap();
        let expect = node::ProgramPart::Decl(
            node::Declaration::Function(
                node::Function { 
                    id: Some("main".into()), 
                    params: vec![], 
                    body: vec![
                        node::ProgramPart::Decl(
                            node::Declaration::Variable(
                                node::VariableKind::Let, 
                                vec![
                                    node::VariableDecl { 
                                        id: node::Pattern::Array(vec![
                                            Some(node::ArrayPatternPart::Patt(
                                                node::Pattern::Identifier("first".into()))), 
                                            Some(node::ArrayPatternPart::Patt(
                                                node::Pattern::Identifier("second".into()))), 
                                            Some(node::ArrayPatternPart::Patt(
                                                node::Pattern::Identifier("third".into())))
                                        ]), 
                                        init: Some(node::Expression::Array(vec![
                                            Some(node::Expression::number("8")), 
                                            Some(node::Expression::number("8")), 
                                            Some(node::Expression::number("8"))
                                            ])) 
                                    }]
                            ))
                    ], 
                    generator: false, 
                    is_async: false 
                }));
        assert_eq!(expect, out)
    }

    #[test]
    fn func() {
        let mut rust = r#"fn thing<T>(&self, stuff: u8, other: T, (a, b): (u8, u8)) -> u8 { 
            let x = 0;
            x 
        }"#;
        let parsed = parse_file(&mut rust).unwrap();
        let expected = node::ProgramPart::Decl(node::Declaration::Function(node::Function{
            id: Some("thing".to_string()),
            is_async: false,
            generator: false,
            params: vec![
                node::FunctionArg::Pattern(node::Pattern::ident("stuff")),
                node::FunctionArg::Pattern(node::Pattern::ident("other")),
                node::FunctionArg::Pattern(node::Pattern::Array(vec![
                    Some(node::ArrayPatternPart::Patt(node::Pattern::ident("a"))),
                    Some(node::ArrayPatternPart::Patt(node::Pattern::ident("b"))),
                ]))
            ],
            body: vec![
                node::ProgramPart::Decl(node::Declaration::Variable(node::VariableKind::Let, vec![node::VariableDecl::with_value("x", node::Expression::number("0"))])),
                node::ProgramPart::Statement(node::Statement::Return(Some(node::Expression::ident("x")))),
            ]
        }));
        let item = parsed.items.iter().next().unwrap();
        println!("{:#?}", item);
        assert_eq!(Some(expected), convert_item(&item));
    }

    #[test]
    fn path() {
        let mut rust = "fn thing() {
            a::b::c::d::e::f::g
        }";
        let parsed = parse_file(&mut rust).unwrap();
        let item = parsed.items.iter().next().unwrap();
        let item = convert_item(&item);
        let ret = node::Expression::member(
                node::Expression::member(
                    node::Expression::member(
                        node::Expression::member(
                            node::Expression::member(
                                node::Expression::member(
                                    node::Expression::ident("a"),
                                    node::Expression::ident("b"),
                                    false),
                            node::Expression::ident("c"),
                            false),
                        node::Expression::ident("d"),
                        false),
                    node::Expression::ident("e"),
                    false),
                node::Expression::ident("f"),
                false),
            node::Expression::ident("g"),
            false);
        let expectations = node::ProgramPart::Decl(
            node::Declaration::Function(
                node::Function {
                    id: Some("thing".into()),
                    params: vec![],
                    is_async: false,
                    generator: false,
                    body: vec![
                        node::ProgramPart::Statement(
                            node::Statement::Return(
                                Some(
                                    ret
                                )
                            )
                        )
                    ]
                }
            )
        );
        assert_eq!(Some(expectations), item)
    }

    #[test]
    fn let_expr() {
        let mut rust = "fn main() {
    if let Some(x) = y { return; }
}";
        let parsed = parse_file(&mut rust).unwrap();
        let item = parsed.items.iter().next().unwrap();
        println!("{:#?}", item);
    }

    #[test]
    fn struct_item() {
        let rust = "struct Thing {
    a: u8,
    b: String,
}
struct Stuff(u8, String);
struct Places;";
        let parsed = parse_file(rust).unwrap();
        let expected = vec![
            node::ProgramPart::Decl(node::Declaration::Class(node::Class::new(Some("Thing"), None, vec![
                node::Property {
                    key: node::PropertyKey::Expr(node::Expression::ident("constructor")),
                    computed: false,
                    short_hand: false,
                    kind: node::PropertyKind::Ctor,
                    method: true,
                    value: node::PropertyValue::Expr(
                        node::Expression::Function(
                            node::Function {
                                id: None,
                                is_async: false,
                                generator: false,
                                params: vec![
                                    node::FunctionArg::Expr(node::Expression::ident("a")),
                                    node::FunctionArg::Expr(node::Expression::ident("b")),
                                ],
                                body: vec![
                                    node::ProgramPart::Statement(
                                        node::Statement::Expr(
                                            node::Expression::Assignment(
                                                node::AssignmentExpression {
                                                    left: node::AssignmentLeft::Expr(Box::new(node::Expression::member(node::Expression::ident("this"), node::Expression::ident("a"), false))),
                                                    operator: node::AssignmentOperator::Equal,
                                                    right: Box::new(node::Expression::ident("a")),
                                                }
                                            )
                                        )
                                    ),
                                    node::ProgramPart::Statement(
                                        node::Statement::Expr(
                                            node::Expression::Assignment(
                                                node::AssignmentExpression {
                                                    left: node::AssignmentLeft::Expr(Box::new(node::Expression::member(node::Expression::ident("this"), node::Expression::ident("b"), false))),
                                                    operator: node::AssignmentOperator::Equal,
                                                    right: Box::new(node::Expression::ident("b")),
                                                }
                                            )
                                        )
                                    )
                                ]
                            }))
                        }
                    ]))),
            node::ProgramPart::Decl(
                node::Declaration::Class(
                    node::Class::new(Some("Stuff".into()), None, vec![
                        node::Property {
                            key: node::PropertyKey::Expr(node::Expression::ident("constructor")),
                            computed: false,
                            short_hand: false,
                            kind: node::PropertyKind::Ctor,
                            method: true,
                            value: node::PropertyValue::Expr(
                                node::Expression::Function(
                                    node::Function {
                                        id: None,
                                        is_async: false,
                                        generator: false,
                                        params: vec![
                                            node::FunctionArg::Expr(node::Expression::ident("_0")),
                                            node::FunctionArg::Expr(node::Expression::ident("_1")),
                                        ],
                                        body: vec![
                                            node::ProgramPart::Statement(node::Statement::Expr(node::Expression::Assignment(node::AssignmentExpression {
                                                left: node::AssignmentLeft::Expr(
                                                    Box::new(node::Expression::member(node::Expression::ident("this"), node::Expression::number("0"), true))
                                                ),
                                                operator: node::AssignmentOperator::Equal,
                                                right: Box::new(node::Expression::ident("_0")),
                                            }))),
                                            node::ProgramPart::Statement(node::Statement::Expr(node::Expression::Assignment(node::AssignmentExpression {
                                                left: node::AssignmentLeft::Expr(
                                                    Box::new(node::Expression::member(node::Expression::ident("this"), node::Expression::number("1"), true))
                                                ),
                                                operator: node::AssignmentOperator::Equal,
                                                right: Box::new(node::Expression::ident("_1")),
                                            }))),
                                        ]
                                    }
                                )
                            )
                        }
                    ])
                )
            ),
            node::ProgramPart::Decl(node::Declaration::Class(node::Class::new(Some("Places"), None, vec![node::Property {
                key: node::PropertyKey::Expr(node::Expression::ident("constructor")),
                computed: false,
                short_hand: false,
                kind: node::PropertyKind::Ctor,
                method: true,
                value: node::PropertyValue::Expr(node::Expression::Function(
                    node::Function {
                        id: None,
                        is_async: false,
                        generator: false,
                        params: vec![],
                        body: vec![],
                    }))
            }])))
        ];
            for (lhs, rhs) in parsed.items.iter().zip(expected) {
                assert_eq!(convert_item(lhs), Some(rhs))
            }
    }
}
