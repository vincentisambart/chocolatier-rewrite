use crate::ast;
use std::collections::HashMap;

#[derive(Debug)]
pub struct TypeIndex {
    pub enums: HashMap<ast::TagId, ast::EnumDef>,
    pub structs: HashMap<ast::TagId, ast::RecordDef>,
    pub unions: HashMap<ast::TagId, ast::RecordDef>,
    pub typedefs: HashMap<String, ast::TypedefDecl>,
    pub protocols: HashMap<String, ast::ProtocolDef>,
    pub interfaces: HashMap<String, ast::InterfaceDef>,
    pub categories: HashMap<String, Vec<ast::CategoryDef>>,
    pub functions: HashMap<String, ast::FuncDecl>,
    pub variables: HashMap<String, ast::VarDecl>,
}

impl TypeIndex {
    pub fn new(items: &[ast::AttributedItem]) -> Self {
        use ast::{Item, RecordKind};

        let mut enums = HashMap::new();
        let mut structs = HashMap::new();
        let mut unions = HashMap::new();
        let mut typedefs = HashMap::new();
        let mut protocols = HashMap::new();
        let mut interfaces = HashMap::new();
        let mut categories = HashMap::new();
        let mut functions = HashMap::new();
        let mut variables = HashMap::new();

        for item in items {
            match &item.item {
                Item::ProtocolDef(def) => {
                    protocols.insert(def.name.clone(), def.clone());
                }
                Item::InterfaceDef(def) => {
                    interfaces.insert(def.name.clone(), def.clone());
                }
                Item::CategoryDef(def) => {
                    let vec = categories.entry(def.class.clone()).or_insert_with(Vec::new);
                    vec.push(def.clone());
                }
                Item::RecordDef(def) => match def.kind {
                    RecordKind::Union => {
                        unions.insert(def.id.clone(), def.clone());
                    }
                    RecordKind::Struct => {
                        structs.insert(def.id.clone(), def.clone());
                    }
                },
                Item::EnumDef(def) => {
                    enums.insert(def.id.clone(), def.clone());
                }
                Item::TypedefDecl(decl) => {
                    typedefs.insert(decl.name.clone(), decl.clone());
                }
                Item::FuncDecl(decl) => {
                    functions.insert(decl.name.clone(), decl.clone());
                }
                Item::VarDecl(decl) => {
                    variables.insert(decl.name.clone(), decl.clone());
                }
            }
        }

        TypeIndex {
            enums,
            structs,
            unions,
            typedefs,
            protocols,
            interfaces,
            categories,
            functions,
            variables,
        }
    }
}
