use crate::{
    context::Context,
    domain,
    hir::{self, TermData},
    typing, Type,
};

macro_rules! map {
    ($($key:expr => $val:expr),* $(,)?) => {{
        // move to arrays if IntoIterator is stabilized
        vec![
            $(
                ($key.into(), $val),
            )*
        ]
        .into_iter()
        .collect()
    }};
}

pub fn env<'a>(ctx: &'a Context<'a>) -> (hir::AlphaEnv<'a>, typing::Environment) {
    let abort_id = hir::Id::new();
    let base_name_of_id = hir::Id::new();
    let derivation_id = hir::Id::new();
    let dir_of_id = hir::Id::new();
    let false_id = hir::Id::new();
    // let fetch_tarball_id = hir::Id::new();
    let import_id = hir::Id::new();
    let is_null_id = hir::Id::new();
    let map_id = hir::Id::new();
    let remove_attrs_id = hir::Id::new();
    let throw_id = hir::Id::new();
    let to_string_id = hir::Id::new();
    let true_id = hir::Id::new();

    let (builtins_id, builtins_ty) = {
        let builtins_id = hir::Id::new();
        let builtins_ty = Type::attr_set(domain::AttrSetType {
            // CR pandaman: polymorphic types, number types (integer | float), missing attrs
            attrs: map![
                "abort" => Type::fun(Type::any(), Type::none()),
                "add" => Type::fun(Type::integer(), Type::fun(Type::integer(), Type::integer())),
                "all" => Type::fun(Type::fun(Type::any(), Type::boolean()), Type::boolean()),
                "any" => Type::fun(Type::fun(Type::any(), Type::boolean()), Type::boolean()),
                "attrNames" => Type::fun(Type::any_attr_set(), Type::list()),
                "attrValues" => Type::fun(Type::any_attr_set(), Type::list()),
                "baseNameOf" => Type::fun(Type::string(), Type::string()),
                "bitAnd" => Type::fun(Type::integer(), Type::fun(Type::integer(), Type::integer())),
                "bitOr" => Type::fun(Type::integer(), Type::fun(Type::integer(), Type::integer())),
                "bitXor" => Type::fun(Type::integer(), Type::fun(Type::integer(), Type::integer())),
                "compareVersions" => Type::fun(Type::string(), Type::fun(Type::string(), Type::integer())),
                "concatLists" => Type::fun(Type::list(), Type::list()),
                "concatMap" => Type::fun(Type::fun(Type::any(), Type::list()), Type::fun(Type::list(), Type::list())),
                "concatStringsSep" => Type::fun(Type::string(), Type::fun(Type::list(), Type::string())),
                "currentSystem" => Type::string(),
                "currentTime" => Type::integer(),
                "deepSeq" => Type::fun(Type::any(), Type::fun(Type::any(), Type::any())),
                "derivation" => Type::fun(Type::any_attr_set(), Type::any_attr_set()),
                "dirOf" => Type::fun(Type::string(), Type::string()),
                "div" => Type::fun(Type::integer(), Type::fun(Type::integer(), Type::integer())),
                "elem" => Type::fun(Type::any(), Type::fun(Type::list(), Type::boolean())),
                "elemAt" => Type::fun(Type::list(), Type::fun(Type::integer(), Type::any())),
                "false" => Type::ff(),
                // "fetchurl" => Type::fun(Type::url(), Type::path()),
                // "fetchTarball" => Type::fun(Type::url(), Type::path()),
                "fetchGit" => Type::fun(Type::any_attr_set(), Type::path()),
                "filter" => Type::fun(Type::fun(Type::any(), Type::boolean()), Type::fun(Type::list(), Type::list())),
                "filterSource" => Type::fun(Type::fun(Type::path(), Type::fun(Type::string(), Type::boolean())), Type::path()),
                "foldl'" => Type::fun(Type::fun(Type::any(), Type::fun(Type::any(), Type::any())), Type::fun(Type::any(), Type::fun(Type::list(), Type::any()))),
                "functionArgs" => Type::fun(Type::fun(Type::any_attr_set(), Type::any()), Type::any_attr_set()),
                "fromJSON" => Type::fun(Type::string(), Type::any()),
                "genList" => Type::fun(Type::fun(Type::integer(), Type::any()), Type::fun(Type::integer(), Type::list())),
                "getAttr" => Type::fun(Type::string(), Type::fun(Type::any_attr_set(), Type::any())),
                "getEnv" => Type::fun(Type::string(), Type::string()),
                "hasAttr" => Type::fun(Type::string(), Type::fun(Type::any_attr_set(), Type::boolean())),
                "hashString" => Type::fun(Type::string(), Type::fun(Type::path(), Type::string())),
                "head" => Type::fun(Type::list(), Type::any()),
                "import" => Type::fun(Type::path(), Type::any()),
                "intersectAttrs" => Type::fun(Type::any_attr_set(), Type::fun(Type::any_attr_set(), Type::any_attr_set())),
                "isAttrs" => Type::fun(Type::any(), Type::boolean()),
                "isBool" => Type::fun(Type::any(), Type::boolean()),
                "isFloat" => Type::fun(Type::any(), Type::boolean()),
                "isFunction" => Type::fun(Type::any(), Type::boolean()),
                "isInt" => Type::fun(Type::any(), Type::boolean()),
                "isFloat" => Type::fun(Type::any(), Type::boolean()),
                "isList" => Type::fun(Type::any(), Type::boolean()),
                "isNull" => Type::fun(Type::any(), Type::boolean()),
                "isPath" => Type::fun(Type::any(), Type::boolean()),
                "isString" => Type::fun(Type::any(), Type::boolean()),
                "langVersion" => Type::integer(),
                "length" => Type::fun(Type::list(), Type::integer()),
                "lessThan" => Type::fun(Type::any(), Type::fun(Type::any(), Type::boolean())),
                "listToAttrs" => Type::fun(Type::list(), Type::any_attr_set()),
                "map" => Type::fun(Type::fun(Type::any(), Type::any()), Type::fun(Type::list(), Type::list())),
                "match" => Type::fun(Type::string(), Type::fun(Type::string(), Type::any())),
                "mul" => Type::fun(Type::integer(), Type::fun(Type::integer(), Type::integer())),
                "nixPath" => Type::list(),
                "nixVersion" => Type::string(),
                // "null" => Type::null(),
                "parseDrvName" => Type::fun(Type::string(), Type::any_attr_set()),
                "path" => Type::fun(Type::any_attr_set(), Type::path()),
                "pathExists" => Type::fun(Type::path(), Type::boolean()),
                "placeholder" => Type::fun(Type::string(), Type::string()),
                "readDir" => Type::fun(Type::path(), Type::any_attr_set()),
                "readFile" => Type::fun(Type::path(), Type::string()),
                "removeAttrs" => Type::fun(Type::any_attr_set(), Type::fun(Type::list(), Type::any_attr_set())),
                "replaceStrings" => Type::fun(Type::list(), Type::fun(Type::list(), Type::fun(Type::string(), Type::string()))),
                "seq" => Type::fun(Type::any(), Type::fun(Type::any(), Type::any())),
                "sort" => Type::fun(Type::fun(Type::any(), Type::fun(Type::any(), Type::any())), Type::fun(Type::list(), Type::list())),
                "split" => Type::fun(Type::string(), Type::fun(Type::string(), Type::list())),
                "splitVersion" => Type::fun(Type::string(), Type::list()),
                "storeDir" => Type::string(),
                "stringLength" => Type::fun(Type::string(), Type::integer()),
                "sub" => Type::fun(Type::integer(), Type::fun(Type::integer(), Type::integer())),
                "substring" => Type::fun(Type::integer(), Type::fun(Type::integer(), Type::fun(Type::string(), Type::string()))),
                "tail" => Type::fun(Type::list(), Type::list()),
                "throw" => Type::fun(Type::string(), Type::none()),
                "toFile" => Type::fun(Type::string(), Type::fun(Type::string(), Type::path())),
                "toJSON" => Type::fun(Type::any(), Type::string()),
                "toPath" => Type::fun(Type::string(), Type::path()),
                "toString" => Type::fun(Type::any(), Type::string()),
                "toXML" => Type::fun(Type::any(), Type::string()),
                "trace" => Type::fun(Type::any(), Type::fun(Type::any(), Type::any())),
                "tryEval" => Type::fun(Type::any(), Type::any_attr_set()),
                "typeOf" => Type::fun(Type::any(), Type::string()),
            ],
            rest: Type::none().into(),
        });

        (builtins_id, builtins_ty)
    };

    (
        map![
            "abort" => ctx.mk_term(TermData::Var(abort_id)),
            "baseNameOf" => ctx.mk_term(TermData::Var(base_name_of_id)),
            "derivation" => ctx.mk_term(TermData::Var(derivation_id)),
            "dirOf" => ctx.mk_term(TermData::Var(dir_of_id)),
            "false" => ctx.mk_term(TermData::Var(false_id)),
            "import" => ctx.mk_term(TermData::Var(import_id)),
            "isNull" => ctx.mk_term(TermData::Var(is_null_id)),
            "map" => ctx.mk_term(TermData::Var(map_id)),
            "removeAttrs" => ctx.mk_term(TermData::Var(remove_attrs_id)),
            "throw" => ctx.mk_term(TermData::Var(throw_id)),
            "toString" => ctx.mk_term(TermData::Var(to_string_id)),
            "true" => ctx.mk_term(TermData::Var(true_id)),
            "builtins" => ctx.mk_term(TermData::Var(builtins_id)),
        ],
        map![
            abort_id => Type::fun(Type::any(), Type::none()),
            base_name_of_id => Type::fun(Type::string(), Type::string()),
            derivation_id => Type::fun(Type::any_attr_set(), Type::any_attr_set()),
            dir_of_id => Type::fun(Type::string(), Type::string()),
            false_id => Type::ff(),
            import_id => Type::fun(Type::path(), Type::any()),
            is_null_id => Type::fun(Type::any(), Type::boolean()),
            map_id => Type::fun(Type::fun(Type::any(), Type::any()), Type::fun(Type::list(), Type::list())),
            remove_attrs_id => Type::fun(Type::any_attr_set(), Type::fun(Type::list(), Type::any_attr_set())),
            throw_id => Type::fun(Type::string(), Type::none()),
            to_string_id => Type::fun(Type::any(), Type::string()),
            true_id => Type::tt(),
            builtins_id => builtins_ty,
        ],
    )
}
