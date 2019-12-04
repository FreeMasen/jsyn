use resast::prelude::*;
use syn::{Expr as SynExpr, Item as SynItem, ItemStatic, Lit as SynLit, ItemConst};

pub fn convert_item(item: &SynItem) -> Option<ProgramPart> {
    match item {
        SynItem::Static(ref item) => Some(convert_static_item(item)),
        SynItem::Const(ref item) => Some(convert_const_item(item)),
        SynItem::Fn(ref item) => Some(ProgramPart::Decl(Decl::Func(convert_fn(item)))),
        SynItem::Struct(ref item) => Some(ProgramPart::Decl(Decl::Class(convert_struct_item(item)))),
        _ => None,
    }
}

fn convert_static_item(item: &ItemStatic) -> ProgramPart {
    let variable_kind = if let Some(_) = &item.mutability {
        VarKind::Let
    } else {
        VarKind::Const
    };
    let id = Pat::Ident(Ident::new(item.ident.to_string()));
    let init = Some(convert_expr(&item.expr));
    ProgramPart::Decl(Decl::Var(
        variable_kind,
        vec![VarDecl { id, init }],
    ))
}

fn convert_const_item(item: &ItemConst) -> ProgramPart {
    let variable_kind = VarKind::Const;
    let id = Pat::Ident(Ident::new(item.ident.to_string()));
    let init = Some(convert_expr(&item.expr));
    ProgramPart::Decl(Decl::Var(
        variable_kind,
        vec![VarDecl { id, init }]
    ))
}

fn convert_fn(item: &syn::ItemFn) -> Func {
    let is_async = item.sig.asyncness.is_some();
    let id = item.sig.ident.to_string();
    let params: Vec<FuncArg> = item.sig.inputs.iter().filter_map(|a| {
        match a {
            syn::FnArg::Typed(ref t) => Some(FuncArg::Pat(convert_pat(&t.pat))),
            _ => None,
        }
    }).collect();
    let body: Vec<ProgramPart> = item.block.stmts.iter().filter_map(|s| {
        match s {
            syn::Stmt::Expr(ref e) => Some(ProgramPart::Stmt(Stmt::Return(Some(convert_expr(e))))),
            syn::Stmt::Item(ref i) => convert_item(i),
            syn::Stmt::Local(ref l) => Some(ProgramPart::Decl(Decl::Var(VarKind::Let, vec![
                    VarDecl {
                        id: convert_pat(&l.pat),
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
    Func {
        id: Some(Ident::new(id)),
        generator: false,
        is_async,
        params,
        body: FuncBody(body),
    }
}

fn convert_struct_item(item: &syn::ItemStruct) -> Class {
    let id = item.ident.to_string();
    let body = convert_struct_fields(&item.fields);
    Class {
        id: Some(Ident::new(id)),
        super_class: None,
        body: ClassBody(vec![body]),
    }
}

fn convert_struct_fields(f: &syn::Fields) -> Prop {
    let key = PropKey::Expr(Expr::Ident(Ident::from("constructor")));
    let value: Func = match f {
        syn::Fields::Named(ref named) => {
            let idents: Vec<Expr> = named.named.iter().map(|f| Expr::Ident(Ident::new(f.ident.clone().unwrap().to_string()))).collect();
            Func {
                id: None,
                is_async: false,
                generator: false,
                params: idents.iter().map(|i| FuncArg::Expr(i.clone())).collect(),
                body: FuncBody(idents.iter().map(|e| {
                    ProgramPart::Stmt(
                        Stmt::Expr(
                            Expr::Assign(
                                AssignExpr {
                                    left: AssignLeft::Expr(
                                        Box::new(Expr::Member(MemberExpr {
                                            object: Box::new(Expr::Ident(Ident::from("this"))),
                                            property: Box::new(e.clone()),
                                            computed: false,
                                        }))
                                    ),
                                    operator: AssignOp::Equal,
                                    right: Box::new(e.clone())
                                }
                            )
                        )
                    )
                }).collect()),
            }        
        },
        syn::Fields::Unnamed(ref unnamed) => {
            let idxs: Vec<usize> = unnamed.unnamed.iter().enumerate().map(|(i, _)| i).collect();
            Func {
                id: None,
                is_async: false,
                generator: false,
                params: idxs.iter().map(|i| FuncArg::Expr(Expr::Ident(Ident::new(format!("_{}", i))))).collect(),
                body: FuncBody(idxs.iter().map(|i| ProgramPart::Stmt(
                    Stmt::Expr(
                        Expr::Assign(
                            AssignExpr {
                                left: AssignLeft::Expr(
                                    Box::new(Expr::Member(MemberExpr {
                                        object: Box::new(Expr::Ident(Ident::from("this"))), 
                                        property: Box::new(Expr::Lit(Lit::Number(std::borrow::Cow::Owned(i.to_string())))),
                                        computed: true
                                    })),
                                ),
                                operator: AssignOp::Equal,
                                right: Box::new(Expr::Ident(Ident::new(format!("_{}", i))))
                            }
                        )
                    )
                )).collect())
            }
        },
        syn::Fields::Unit => Func {
            id: None,
            is_async: false,
            generator: false,
            params: vec![],
            body: FuncBody(vec![]),
        },
    };
    
    Prop {
        key,
        computed: false,
        kind: PropKind::Ctor,
        method: true,
        short_hand: false,
        is_static: false,
        value: PropValue::Expr(Expr::Func(value)),
    }
}

fn convert_expr(expr: &SynExpr) -> Expr {
    match expr {
        SynExpr::Array(ref array) => Expr::Array(
            array
                .elems
                .iter()
                .map(convert_expr)
                .map(|e| Some(e))
                .collect(),
        ),
        SynExpr::Lit(ref lit) => convert_literal(&lit.lit),
        SynExpr::Call(ref call) => convert_call(call),
        SynExpr::MethodCall(ref call) => convert_method_call(call),
        SynExpr::Tuple(ref tup) => convert_tuple(tup),
        SynExpr::Binary(ref bin) => convert_binary(bin),
        SynExpr::Unary(ref un) => convert_unary(un),
        SynExpr::Cast(ref cast) => convert_expr(&cast.expr),
        SynExpr::Type(ref ty) => convert_expr(&ty.expr),
        SynExpr::Let(ref assign) => convert_let_expr(&assign),
        SynExpr::Path(ref p) => {
            let mut exprs: Vec<Expr> = p.path.segments.iter().map(|p| { Expr::Ident(Ident::new(p.ident.to_string()))}).collect();
            if exprs.len() == 1 {
                exprs.pop().unwrap()
            } else {
                let mut items = exprs.into_iter();
                let mut ret = items.next().unwrap();
                for e in items {
                    ret = Expr::Member( MemberExpr {
                        object: Box::new(ret), 
                        property: Box::new(e), 
                        computed: false
                    });
                }
                ret
            }
        },
        SynExpr::If(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::While(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::ForLoop(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Loop(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Match(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Closure(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Unsafe(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Block(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Assign(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::AssignOp(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Field(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Index(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Range(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Reference(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Break(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Continue(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Return(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Macro(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Struct(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Repeat(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Paren(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Group(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Try(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Async(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::TryBlock(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Yield(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Verbatim(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Box(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        SynExpr::Await(ref e) => unimplemented!("Unimplemented Expr: {:#?}", e),
        _ => unimplemented!("Unimplemented Expr: {:#?}", expr),
    }
}

fn convert_call(call: &syn::ExprCall) -> Expr {
    let callee = convert_expr(&call.func);
    let args = call.args.iter().map(convert_expr).collect();
    Expr::Call(CallExpr {
        callee: Box::new(callee), 
        arguments: args,
    })
}

fn convert_method_call(call: &syn::ExprMethodCall) -> Expr {
    let object = convert_expr(&call.receiver);
    let property = Expr::Ident(Ident::new(call.method.to_string()));
    let args = call.args.iter().map(convert_expr).collect();
    let callee = member_expr(object, property, false);
    Expr::Call(CallExpr {
        callee: Box::new(callee), 
        arguments: args
    })
}

fn convert_tuple(tup: &syn::ExprTuple) -> Expr {
    let elements = tup.elems
                .iter()
                .map(convert_expr)
                .map(|e| Some(e))
                .collect();
    Expr::Array(elements)
}

fn convert_literal(literal: &SynLit) -> Expr {
    match literal {
        SynLit::Str(ref s) => Expr::Lit(Lit::String(StringLit::Double(
            std::borrow::Cow::Owned(format!("{}", s.value()))))),
        SynLit::Byte(ref b) => Expr::Lit(Lit::Number(std::borrow::Cow::Owned(format!("{}", b.value())))),
        SynLit::ByteStr(ref bs) => Expr::Array(bs.value().iter().map(|b| Some(Expr::Lit(Lit::Number(std::borrow::Cow::Owned(format!("{}", b)))))).collect()),
        SynLit::Char(ref c) => Expr::Lit(Lit::String(StringLit::Single(std::borrow::Cow::Owned(format!("{}", c.value()))))),
        SynLit::Int(ref int) => Expr::Lit(Lit::Number(std::borrow::Cow::Owned(format!("{}", int.base10_digits())))),
        SynLit::Float(ref f) => Expr::Lit(Lit::Number(std::borrow::Cow::Owned(format!("{}", f.base10_digits())))),
        SynLit::Bool(ref b) => Expr::Lit(Lit::Boolean(b.value)),
        SynLit::Verbatim(ref l) => Expr::Lit(Lit::Number(std::borrow::Cow::Owned(format!("{}", l)))),
    }
}

#[derive(Clone, Debug)]
enum BinOp {
    Binary(BinaryOp),
    Logical(LogicalOp),
    Assign(AssignOp),
}

fn convert_binary(binary: &syn::ExprBinary) -> Expr {
    let left = convert_expr(&binary.left);
    let op = convert_operator(&binary.op);
    let right = convert_expr(&binary.right);
    match op {
        BinOp::Binary(op) => Expr::Binary(BinaryExpr {
            left: Box::new(left), 
            right: Box::new(right),
            operator: op,
        }),
        BinOp::Logical(op) => Expr::Logical(LogicalExpr {
            left: Box::new(left),
            right: Box::new(right),
            operator: op,
        }),
        BinOp::Assign(op) => Expr::Assign(AssignExpr {
            left: AssignLeft::Expr(Box::new(left)), 
            right: Box::new(right),
            operator: op,
        }),
    }
}

fn convert_unary(op: &syn::ExprUnary) -> Expr {
    match &op.op {
        syn::UnOp::Not(_) => {
            let expr = convert_expr(&op.expr);
            Expr::Unary(UnaryExpr {
                operator: UnaryOp::Not,
                prefix: true, 
                argument: Box::new(expr),
            })
        },
        syn::UnOp::Neg(_) => {
            let expr = convert_expr(&op.expr);
            Expr::Unary(UnaryExpr {
                operator: UnaryOp::Minus,
                prefix: true,
                argument: Box::new(expr),
            })
        },
        _ => convert_expr(&op.expr)
    }
}

fn convert_operator(op: &syn::BinOp) -> BinOp {
    match op {
        syn::BinOp::Add(_) => BinOp::Binary(BinaryOp::Plus),
        syn::BinOp::Sub(_) => BinOp::Binary(BinaryOp::Minus),
        syn::BinOp::Mul(_) => BinOp::Binary(BinaryOp::Times),
        syn::BinOp::Div(_) => BinOp::Binary(BinaryOp::Over),
        syn::BinOp::Rem(_) => BinOp::Binary(BinaryOp::Mod),
        syn::BinOp::And(_) => BinOp::Logical(LogicalOp::And),
        syn::BinOp::Or(_) => BinOp::Logical(LogicalOp::Or),
        syn::BinOp::BitXor(_) => BinOp::Binary(BinaryOp::XOr),
        syn::BinOp::BitAnd(_) => BinOp::Binary(BinaryOp::And),
        syn::BinOp::BitOr(_) => BinOp::Binary(BinaryOp::Or),
        syn::BinOp::Shl(_) => BinOp::Binary(BinaryOp::LeftShift),
        syn::BinOp::Shr(_) => BinOp::Binary(BinaryOp::RightShift),
        syn::BinOp::Eq(_) => BinOp::Binary(BinaryOp::Equal),
        syn::BinOp::Lt(_) => BinOp::Binary(BinaryOp::LessThan),
        syn::BinOp::Le(_) => BinOp::Binary(BinaryOp::LessThanEqual),
        syn::BinOp::Ne(_) => BinOp::Binary(BinaryOp::NotEqual),
        syn::BinOp::Ge(_) => BinOp::Binary(BinaryOp::GreaterThanEqual),
        syn::BinOp::Gt(_) => BinOp::Binary(BinaryOp::GreaterThan),
        syn::BinOp::AddEq(_) => BinOp::Assign(AssignOp::PlusEqual),
        syn::BinOp::SubEq(_) => BinOp::Assign(AssignOp::MinusEqual),
        syn::BinOp::MulEq(_) => BinOp::Assign(AssignOp::TimesEqual),
        syn::BinOp::DivEq(_) => BinOp::Assign(AssignOp::DivEqual),
        syn::BinOp::RemEq(_) => BinOp::Assign(AssignOp::ModEqual),
        syn::BinOp::BitXorEq(_) => BinOp::Assign(AssignOp::XOrEqual),
        syn::BinOp::BitAndEq(_) => BinOp::Assign(AssignOp::AndEqual),
        syn::BinOp::BitOrEq(_) => BinOp::Assign(AssignOp::OrEqual),
        syn::BinOp::ShlEq(_) => BinOp::Assign(AssignOp::LeftShiftEqual),
        syn::BinOp::ShrEq(_) => BinOp::Assign(AssignOp::RightShiftEqual),
    }
}

fn convert_let_expr(expr: &syn::ExprLet) -> Expr {
    let _kind = VarKind::Let;
    let _left = convert_pat(&expr.pat);
    let _op = AssignOp::Equal;
    unimplemented!("convert_let_expr {:#?}", expr);
}
fn convert_pat_type(pat_ty: &syn::PatType) -> Pat {
    convert_pat(&*pat_ty.pat)
}
fn convert_pat(pat: &syn::Pat) -> Pat {
    match pat {
        syn::Pat::Wild(_) => Pat::Ident(Ident::from("_")),
        syn::Pat::Ident(ref i) => Pat::Ident(Ident::new(i.ident.to_string())),
        syn::Pat::Struct(ref s) => {
            let parts = s.fields.iter().map(|f| ObjPatPart::Assign(Prop {
                key: PropKey::Pat(convert_pat(&f.pat)),
                value: PropValue::None,
                method: false,
                is_static: false,
                kind: PropKind::Init,
                short_hand: true,
                computed: false,
            })).collect();
            Pat::Obj(parts)
        },
        syn::Pat::Tuple(ref tup) => {
            let parts = tup.elems.iter().map(pat_to_array_part).collect();
            Pat::Array(parts)
        },
        syn::Pat::Box(ref expr) => convert_pat(&expr.pat),
        syn::Pat::Reference(ref r) => convert_pat(&r.pat),
        syn::Pat::Slice(ref s) => {
            let mut parts: Vec<Option<ArrayPatPart>> = s.elems.iter().map(pat_to_array_part).collect();
            Pat::Array(parts)
        },
        _ => unimplemented!("Unimplemented Pat {:#?}", pat),
    }
}

fn pat_to_array_part(pat: &syn::Pat) -> Option<ArrayPatPart> {
    Some(ArrayPatPart::Pat(convert_pat(pat)))
} 

fn member_expr<'a>(object: Expr<'a>, property: Expr<'a>, computed: bool) -> Expr<'a> {
    Expr::Member(MemberExpr {
        object: Box::new(object),
        property: Box::new(property),
        computed,
    })
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
                ProgramPart::Decl(Decl::Var(
                    VarKind::Const,
                    vec![VarDecl {
                        id: Pat::ident_from("X"),
                        init: Some(Expr::Array(vec![
                            Some(Expr::Lit(Lit::number_from("0"))),
                            Some(Expr::Lit(Lit::number_from("1"))),
                            Some(Expr::Lit(Lit::number_from("2"))),
                        ])),
                    }
                    ]
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
                ProgramPart::Decl(Decl::Var(
                    VarKind::Const,
                    vec![VarDecl {
                        id: Pat::ident_from("X"),
                        init: Some(Expr::Array(vec![
                            Some(Expr::Lit(Lit::number_from("0"))),
                            Some(Expr::Lit(Lit::number_from("1"))),
                            Some(Expr::Lit(Lit::number_from("2"))),
                        ])),
                    }
                    ]
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
                ProgramPart::Decl(Decl::Var(
                    VarKind::Const,
                    vec![
                        VarDecl {
                            id: Pat::ident_from("X"),
                            init: Some(Expr::Lit(Lit::number_from("0")))
                        }
                    ]
                ))
            )
        }
    }

    #[test]
    fn pat_obj() {
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
        let expect = ProgramPart::Decl(
            Decl::Func(
                Func { 
                    id: Some(Ident::from("main")), 
                    params: vec![], 
                    body: FuncBody(vec![
                        ProgramPart::Decl(
                            Decl::Var(
                                VarKind::Let, 
                                vec![
                                    VarDecl { 
                                        id: Pat::Array(vec![
                                            Some(ArrayPatPart::Pat(
                                                Pat::ident_from("first"))), 
                                            Some(ArrayPatPart::Pat(
                                                Pat::ident_from("second"))), 
                                            Some(ArrayPatPart::Pat(
                                                Pat::ident_from("third")))
                                        ]), 
                                        init: Some(Expr::Array(vec![
                                            Some(Expr::Lit(Lit::number_from("8"))),
                                            Some(Expr::Lit(Lit::number_from("8"))),
                                            Some(Expr::Lit(Lit::number_from("8"))),
                                            ])) 
                                    }]
                            ))
                    ]),
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
        let expected = ProgramPart::Decl(Decl::Func(Func{
            id: Some(Ident::from("thing")),
            is_async: false,
            generator: false,
            params: vec![
                FuncArg::Pat(Pat::ident_from("stuff")),
                FuncArg::Pat(Pat::ident_from("other")),
                FuncArg::Pat(Pat::Array(vec![
                    Some(ArrayPatPart::Pat(Pat::ident_from("a"))),
                    Some(ArrayPatPart::Pat(Pat::ident_from("b"))),
                ]))
            ],
            body: FuncBody(vec![
                ProgramPart::Decl(Decl::Var(VarKind::Let, vec![
                    VarDecl {
                        id: Pat::ident_from("x"), 
                        init: Some(Expr::Lit(Lit::number_from("0")))
                    }])),
                ProgramPart::Stmt(Stmt::Return(Some(Expr::Ident(Ident::from("x"))))),
            ]),
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
        let ret = member_expr(
            member_expr(
                member_expr(
                    member_expr(
                        member_expr(
                            member_expr(
                                    Expr::Ident(Ident::from("a")),
                                    Expr::Ident(Ident::from("b")),
                                    false),
                            Expr::Ident(Ident::from("c")),
                            false),
                        Expr::Ident(Ident::from("d")),
                        false),
                    Expr::Ident(Ident::from("e")),
                    false),
                Expr::Ident(Ident::from("f")),
                false),
            Expr::Ident(Ident::from("g")),
            false);
        let expectations = ProgramPart::Decl(
            Decl::Func(
                Func {
                    id: Some(Ident::from("thing")),
                    params: vec![],
                    is_async: false,
                    generator: false,
                    body: FuncBody(vec![
                        ProgramPart::Stmt(
                            Stmt::Return(
                                Some(
                                    ret
                                )
                            )
                        )
                    ])
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
            ProgramPart::Decl(Decl::Class(Class::new(Some(Ident::from("Thing")), None, vec![
                Prop {
                    key: PropKey::Expr(Expr::Ident(Ident::from("constructor"))),
                    computed: false,
                    short_hand: false,
                    kind: PropKind::Ctor,
                    method: true,
                    is_static: false,
                    value: PropValue::Expr(
                        Expr::Func(
                            Func {
                                id: None,
                                is_async: false,
                                generator: false,
                                params: vec![
                                    FuncArg::Expr(Expr::Ident(Ident::from("a"))),
                                    FuncArg::Expr(Expr::Ident(Ident::from("b"))),
                                ],
                                body: FuncBody(vec![
                                    ProgramPart::Stmt(
                                        Stmt::Expr(
                                            Expr::Assign(
                                                AssignExpr {
                                                    left: AssignLeft::Expr(Box::new(member_expr(Expr::ident_from("this"), Expr::ident_from("a"), false))),
                                                    operator: AssignOp::Equal,
                                                    right: Box::new(Expr::ident_from("a")),
                                                }
                                            )
                                        )
                                    ),
                                    ProgramPart::Stmt(
                                        Stmt::Expr(
                                            Expr::Assign(
                                                AssignExpr {
                                                    left: AssignLeft::Expr(Box::new(member_expr(Expr::ident_from("this"), Expr::ident_from("b"), false))),
                                                    operator: AssignOp::Equal,
                                                    right: Box::new(Expr::ident_from("b")),
                                                }
                                            )
                                        )
                                    )
                                ])
                            }))
                        }
                    ]))),
            ProgramPart::Decl(
                Decl::Class(
                    Class::new(Some(Ident::from("Stuff")), None, vec![
                        Prop {
                            key: PropKey::Expr(Expr::ident_from("constructor")),
                            computed: false,
                            short_hand: false,
                            kind: PropKind::Ctor,
                            method: true,
                            is_static: false,
                            value: PropValue::Expr(
                                Expr::Func(
                                    Func {
                                        id: None,
                                        is_async: false,
                                        generator: false,
                                        params: vec![
                                            FuncArg::Expr(Expr::ident_from("_0")),
                                            FuncArg::Expr(Expr::ident_from("_1")),
                                        ],
                                        body: FuncBody(vec![
                                            ProgramPart::Stmt(Stmt::Expr(Expr::Assign(AssignExpr {
                                                left: AssignLeft::Expr(
                                                    Box::new(member_expr(Expr::ident_from("this"), Expr::Lit(Lit::Number("0".into())), true))
                                                ),
                                                operator: AssignOp::Equal,
                                                right: Box::new(Expr::Ident(Ident::from("_0"))),
                                            }))),
                                            ProgramPart::Stmt(Stmt::Expr(Expr::Assign(AssignExpr {
                                                left: AssignLeft::Expr(
                                                    Box::new(member_expr(Expr::ident_from("this"), Expr::Lit(Lit::Number("1".into())), true))
                                                ),
                                                operator: AssignOp::Equal,
                                                right: Box::new(Expr::ident_from("_1")),
                                            }))),
                                        ])
                                    }
                                )
                            )
                        }
                    ])
                )
            ),
            ProgramPart::Decl(Decl::Class(Class::new(Some(Ident::from("Places")), None, vec![Prop {
                key: PropKey::Expr(Expr::ident_from("constructor")),
                computed: false,
                short_hand: false,
                kind: PropKind::Ctor,
                is_static: false,
                method: true,
                value: PropValue::Expr(Expr::Func(
                    Func {
                        id: None,
                        is_async: false,
                        generator: false,
                        params: vec![],
                        body: FuncBody(vec![]),
                    }))
            }])))
        ];
            for (lhs, rhs) in parsed.items.iter().zip(expected) {
                assert_eq!(convert_item(lhs), Some(rhs))
            }
    }
}
