use proc_macro2::{Span,TokenStream,Ident};
use quote::quote;
use syn::{
    parse_quote, punctuated::Punctuated, token::Comma, Data, DataStruct, DeriveInput, Field,
    Fields, FieldsNamed, FieldsUnnamed, Lifetime, Stmt,ExprPath,LitStr
};

use super::{
    attributes::{parse_child_attributes, parse_container_attributes,SqlxChildAttributes},
    rename_all,
};

pub fn expand_derive_from_row(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    match &input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => expand_derive_from_row_struct(input, named),

        Data::Struct(DataStruct {
            fields: Fields::Unnamed(FieldsUnnamed { unnamed, .. }),
            ..
        }) => expand_derive_from_row_struct_unnamed(input, unnamed),

        Data::Struct(DataStruct {
            fields: Fields::Unit,
            ..
        }) => Err(syn::Error::new_spanned(
            input,
            "unit structs are not supported",
        )),

        Data::Enum(_) => Err(syn::Error::new_spanned(input, "enums are not supported")),

        Data::Union(_) => Err(syn::Error::new_spanned(input, "unions are not supported")),
    }
}

fn expand_derive_from_row_struct(
    input: &DeriveInput,
    fields: &Punctuated<Field, Comma>,
) -> syn::Result<proc_macro2::TokenStream> {
    let ident = &input.ident;

    let generics = &input.generics;

    let (lifetime, provided) = generics
        .lifetimes()
        .next()
        .map(|def| (def.lifetime.clone(), false))
        .unwrap_or_else(|| (Lifetime::new("'a", Span::call_site()), true));

    let (_, ty_generics, _) = generics.split_for_impl();

    let mut generics = generics.clone();
    generics.params.insert(0, parse_quote!(R: sqlx::Row));

    if provided {
        generics.params.insert(0, parse_quote!(#lifetime));
    }

    let predicates = &mut generics.make_where_clause().predicates;

    predicates.push(parse_quote!(&#lifetime str: sqlx::ColumnIndex<R>));

    let row_fields = get_row_fields(&fields)?;
    for field in row_fields.iter() {
        let ty = &field.ty;

        if field.attrs.try_from.is_none() {
            predicates.push(parse_quote!(#ty: ::sqlx::decode::Decode<#lifetime, R::Database>));
            predicates.push(parse_quote!(#ty: ::sqlx::types::Type<R::Database>));
        }
    }

    let (impl_generics, _, where_clause) = generics.split_for_impl();

    let container_attributes = parse_container_attributes(&input.attrs)?;

    let reads = row_fields
        .iter()
        .filter_map(|field| -> Option<TokenStream> {
            let id = field.ident.as_ref()?;
            let attributes = &field.attrs;
            let ty = &field.ty;
            let id_s = attributes
                .rename
                .clone()
                .or_else(|| Some(id.to_string().trim_start_matches("r#").to_owned()))
                .map(|s| match container_attributes.rename_all {
                    Some(pattern) => rename_all(&s, pattern),
                    None => s,
                })
                .unwrap();

            let with_block = match &attributes.try_from {
                Some(path) => {
                    quote! {
                        let id_val = #path(row).map_err(|source| ::sqlx::Error::ColumnDecode {
                            index: #id_s.to_owned(),
                            source:source.into(),
                        })?;
                    }
                }
                None => {
                    quote! {
                        let id_val = row;
                    }
                }
            };

            let body_block = if attributes.default {
                quote! {
                    let row_res:Option<#ty> = row.try_get(#id_s);
                    let id_val:#ty = match row_res{
                        Err(::sqlx::Error::ColumnNotFound(_)) => Default::default(),
                        row_res => {
                            let row = row_res?;
                            let row = match row{
                                Some(row) => row,
                                None => Default::default(),
                            };
                            #with_block
                            id_val
                        }
                    };
                }
            } else {
                quote! {
                    let row_res = row.try_get(#id_s);
                    let row = row_res?;
                    #with_block
                }
            };

            Some(quote! {
                let #id = {
                    #body_block
                    id_val
                };
            })
        })
        .collect::<Vec<_>>();

    let names = fields.iter().map(|field| &field.ident).collect::<Vec<_>>();

    let mut block = TokenStream::new();

    let mut extend_block = |exprpath: ExprPath| {
        block.extend(quote!(
            #[automatically_derived]
            impl #impl_generics ::sqlx::FromRow<#lifetime, R> for #ident #ty_generics #where_clause,R: ::sqlx::Row<Database = #exprpath>, {
                fn from_row(row: &#lifetime R) -> ::sqlx::Result<Self> {
                    #(#reads)*
                    ::std::result::Result::Ok(#ident {
                        #(#names),*
                    })
                }
            }
        ));
    };

    if cfg!(feature = "mysql") {
        extend_block(LitStr::new("::sqlx::MySql", Span::call_site()).parse()?);
    }
    if cfg!(feature = "postgres") {
        extend_block(LitStr::new("::sqlx::Postgres", Span::call_site()).parse()?);
    }
    if cfg!(feature = "sqlite") {
        extend_block(LitStr::new("::sqlx::Sqlite", Span::call_site()).parse()?);
    }
    if cfg!(feature = "mssql") {
        extend_block(LitStr::new("::sqlx::Mssql", Span::call_site()).parse()?);
    }
    if cfg!(feature = "any") {
        extend_block(LitStr::new("::sqlx::Any", Span::call_site()).parse()?);
    }

    Ok(block)
}

fn expand_derive_from_row_struct_unnamed(
    input: &DeriveInput,
    fields: &Punctuated<Field, Comma>,
) -> syn::Result<proc_macro2::TokenStream> {
    let ident = &input.ident;

    let generics = &input.generics;

    let (lifetime, provided) = generics
        .lifetimes()
        .next()
        .map(|def| (def.lifetime.clone(), false))
        .unwrap_or_else(|| (Lifetime::new("'a", Span::call_site()), true));

    let (_, ty_generics, _) = generics.split_for_impl();

    let mut generics = generics.clone();
    generics.params.insert(0, parse_quote!(R: sqlx::Row));

    if provided {
        generics.params.insert(0, parse_quote!(#lifetime));
    }

    let predicates = &mut generics.make_where_clause().predicates;

    predicates.push(parse_quote!(usize: sqlx::ColumnIndex<R>));

    for field in fields {
        let ty = &field.ty;

        predicates.push(parse_quote!(#ty: sqlx::decode::Decode<#lifetime, R::Database>));
        predicates.push(parse_quote!(#ty: sqlx::types::Type<R::Database>));
    }

    let (impl_generics, _, where_clause) = generics.split_for_impl();

    let gets = fields
        .iter()
        .enumerate()
        .map(|(idx, _)| quote!(row.try_get(#idx)?));

    Ok(quote!(
        impl #impl_generics sqlx::FromRow<#lifetime, R> for #ident #ty_generics #where_clause {
            fn from_row(row: &#lifetime R) -> sqlx::Result<Self> {
                Ok(#ident (
                    #(#gets),*
                ))
            }
        }
    ))
}

struct RowField<'a> {
    attrs: SqlxChildAttributes,
    ident: &'a Option<Ident>,
    ty: &'a syn::Type,
}

fn get_row_fields<'a>(
    fields: &'a Punctuated<syn::Field, syn::Token![,]>,
) -> Result<Vec<RowField<'a>>, syn::Error> {
    fields
        .iter()
        .enumerate()
        .map(|(_, field)| {
            Ok(RowField {
                ident: &field.ident,
                attrs: parse_child_attributes(&field.attrs)?,
                ty: &field.ty,
            })
        })
        .collect()
}
