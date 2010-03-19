package ru.circumflex.orm

import java.util.StringTokenizer
import collection.mutable.ListBuffer

object FormatUtil {
  def WHITESPACE = " \n\t\f\r"
  def DELIMITERS = "()'\",;." + WHITESPACE
  def KEYWORDS = Set("abs", "absolute", "action", "add", "after", "aggregate",
    "all", "allocate", "alter", "analyze", "and", "any", "are", "array", "array_agg",
    "as", "asc", "asensitive", "assertion", "asymmetric", "at", "atomic", "authorization",
    "avg", "before", "begin", "between", "bigint", "binary", "bit", "blob", "boolean",
    "both", "breadth", "by", "cache", "call", "called", "cardinality", "cascade", "case",
    "cast", "catalog", "ceil", "ceiling", "char", "character", "character_length", "char_length",
    "check", "checkpoint", "class", "clob", "close", "coalesce", "collate", "collation",
    "collect", "column", "comment", "commit", "completion", "condition", "connect", "connection",
    "constraint", "constraints", "constructor", "contains", "continue", "copy", "corr",
    "corresponding", "cost", "count", "covar_pop", "covar_samp", "create", "cross", "cube",
    "cume_dist", "current", "current_catalog", "current_date", "current_default_transform_group",
    "current_path", "current_role", "current_schema", "current_time", "current_timestamp",
    "current_transform_group_for_type", "current_user", "cursor", "cycle", "data", "database",
    "date", "day", "deallocate", "dec", "decimal", "declare", "default", "deferrable", "deferred",
    "delete", "dense_rank", "depth", "deref", "desc", "descrive", "descriptor", "destroy",
    "destructor", "deterministic", "diagnostics", "dictionary", "disable", "disconnect", "distinct",
    "do", "document", "domain", "double", "drop", "dynamic", "each", "element", "else", "empty",
    "enable", "encoding", "encrypted", "end", "end-exec", "enum", "equals", "escape", "every",
    "except", "exception", "exclude", "excluding", "exclusive", "exec", "execute", "existing",
    "exists", "exp", "explain", "external", "extract", "false", "fetch", "filter", "first",
    "first_value", "float", "floor", "for", "force", "foreign", "found", "free", "freeze",
    "from", "full", "function", "fusion", "general", "generated", "get", "global", "go",
    "goto", "grant", "granted", "greatest", "group", "grouping", "handler", "having", "hex",
    "hold", "host", "hour", "identity", "if", "ignore", "ilike", "immediate", "implicit",
    "in", "including", "increment", "index", "indicator", "infix", "inherit", "inherits",
    "initialize", "initially", "inner", "inout", "input", "insensitive", "insert", "instance",
    "instead", "int", "integer", "intersect", "intersection", "interval", "into", "invoker", "is",
    "isnull", "isolation", "iterate", "join", "key", "lag", "language", "large", "last",
    "last_value", "lateral", "lc_collate", "lc_ctype", "lead", "leading", "least", "left", "length",
    "less", "level", "like", "like_regex", "limit", "listen", "ln", "load", "local", "localtime",
    "localtimestamp", "location", "locator", "lock", "login", "lower", "map", "mapping", "match",
    "matched", "max", "maxvalue", "max_cardinality", "member", "merge", "method", "min", "minute",
    "mod", "mode", "modifies", "modify", "module", "month", "more", "move", "multiset", "names",
    "namespace", "national", "natural", "nchar", "nclob", "nesting", "new", "next", "nil", "no",
    "none", "normalize", "not", "nothing", "notify", "notnull", "now", "nowait", "nth_value", "ntile",
    "null", "nullable", "nullif", "nulls", "number", "numeric", "object", "occurences_regex",
    "octets", "octet_length", "of", "off", "offset", "oids", "old", "on", "only", "open",
    "operation", "operator", "option", "options", "or", "order", "ordering", "ordinality",
    "others", "out", "outer", "output", "over", "overlaps", "overlay", "overriding", "owned",
    "owner", "pad", "parameter", "parameters", "parser", "partial", "partition", "passing", "password",
    "path", "percentile_cont", "percentile_disc", "percent_rank", "perform", "placing", "plans", "position",
    "position_regex", "postfix", "power", "preceding", "precision", "prefix", "preorder", "prepare",
    "prepared", "preserve", "primary", "prior", "privileges", "procedural", "procedure", "public",
    "quote", "range", "rank", "read", "reads", "real", "reassign", "recheck", "recursive", "ref",
    "references", "referencing", "regr_avgx", "regr_avgy", "regr_count", "regr_intercept", "regr_r2",
    "regr_slope", "regr_sxx", "regr_sxy", "regr_syy", "reindex", "relative", "rename", "repeatable",
    "replace", "replica", "reset", "restart", "restrict", "result", "return", "returning", "returns",
    "revoke", "right", "role", "rollback", "rollup", "routine", "row", "rows", "row_count",
    "row_number", "rule", "savepoint", "scale", "schema", "scope", "scope_catalog", "scroll",
    "search", "second", "section", "select", "self", "sensitive", "sequence", "serializable",
    "server", "session", "session_user", "set", "setof", "sets", "share", "show", "similar",
    "simple", "size", "smallint", "some", "source", "space", "specific", "specifictype", "sql",
    "sqlcode", "sqlerror", "sqlexception", "sqlstate", "sqlwarning", "sqrt", "stable", "standalone",
    "start", "statement", "static", "statistics", "stddev_pop", "stddev_samp", "stdin", "stdout",
    "storage", "strict", "strip", "structure", "style", "sublist", "submultiset", "substring_regex",
    "sum", "symmetric", "system", "system_user", "table", "tablesample", "tablespace", "temp",
    "template", "temporary", "terminate", "text", "than", "then", "ties", "time", "timestamp",
    "timezone_hour", "timezone_minute", "to", "trailing", "transaction", "transform", "transforms",
    "translate", "translate_regex", "translation", "treat", "trigger", "trim", "trim_array", "true",
    "truncate", "trusted", "type", "uescape", "unbounded", "under", "union", "unique", "unknown",
    "unknown", "unlisten", "unnamed", "unnest", "until", "untyped", "update", "upper", "uri",
    "usage", "user", "using", "vacuum", "valid", "validator", "value", "values", "varbinary",
    "varchar", "variable", "variadic", "varying", "var_pop", "var_samp", "verbose", "version",
    "view", "volatile", "when", "whenever", "where", "whitespace", "width_bucket", "window",
    "with", "within", "without", "work", "wrapper", "white", "xml", "xmlagg", "xmlattributes",
    "xmlbinary", "xmlcast", "xmlcomment", "xmlconcat", "xmldocument", "xmlelement", "xmlexists",
    "xmlforest", "xmliterate", "xmlnamespaces", "xmlparse", "xmlpi", "xmlquery", "xmlroot", "xmlschema",
    "xmlserialize", "xmltable", "xmlvalidate", "year", "yes", "zone")

}

class SQLFormatter {

  import FormatUtil._

  protected var _indentString: String = "  "
  protected var _lower = true

  def indent(value: String): this.type = {
    _indentString = value
    return this
  }

  def lower: this.type = {
    _lower = true
    return this
  }

  def upper: this.type = {
    _lower = false
    return this
  }

  // TODO: implement SQL formatting
  def format(sql: String): String = sql

}
