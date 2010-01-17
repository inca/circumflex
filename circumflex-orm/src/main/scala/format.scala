/*
 * Copyright (C) 2009-2010 Boris Okunskiy (http://incarnate.ru)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

package ru.circumflex.orm

import java.util.StringTokenizer

object FormatUtil {
  def WHITESPACE = " \n\t\f\r"
  def DELIMITERS = "()'\"[],;" + WHITESPACE
  def KEYWORDS = Set[String]("abs", "absolute", "action", "add", "after", "aggregate",
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
    "path", "percentile_cont", "percentile_disc", "percent_rank", "placing", "plans", "position",
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

  protected var _indentString: String = "  "
  protected var _terminatorString: String = ";"
  protected var _lower = true

  def indentString = _indentString
  def indent(value: String): this.type = {
    _indentString = value
    return this
  }

  def terminatorString = _terminatorString
  def terminator(value: String): this.type = {
    _terminatorString = value
    return this
  }

  def lower_?() = _lower
  def upper_?() = !_lower
  def lower: this.type = {
    _lower = true
    return this
  }
  def upper: this.type = {
    _lower = false
    return this
  }

  def format(sql: String): String = new Worker(sql).process

  def simpleSelect = format("""
  SELECT a.id, b.id as b_id, COUNT(c.id)
  FROM schema.a AS a LEFT OUTER JOIN schema.b AS b ON a.id = b.a_id
  LEFT OUTER JOIN schema.c AS c ON a.id = c.a_id
  WHERE (a.id <> 0 AND a.id IS NULL) OR (b.id <> 0 AND b.id IS NOT NULL)
  AND c.id IN (
      select st.id from schema.stuff where st.id is not null and st.id > 0
  )
  OR a.name LIKE
'I am multiline text.
I''ma tellin'' you.'
  OR 'I''m tellin'' you what... This should be preserved even if I''ma sElEcT or upDAte or other SQL stuff.' 
  GROUP BY a.id, b.id
  HAVING COUNT(c.id) > 0
  ORDER BY 1 ASC, b.id DESC
  LIMIT 1
  OFFSET 1
  """)

  protected class Worker(sql: String) {

    import FormatUtil._

    val result = new StringBuffer
    val tokenizer = new StringTokenizer(sql.trim, DELIMITERS, true)

    var token = ""
    var lcToken = ""
    var lastToken = ""

    var indent = 1
    var initial = "\n  "

    def process(): String = {
      result.append(initial)
      while(tokenizer.hasMoreTokens) {
        token = tokenizer.nextToken
        lcToken = token.toLowerCase
        // first let's make sure that quoted strings are preserved
        processQuotes
        out()
      }
      return result.toString
    }

    def processQuotes: Unit = token match {
      case "'" => while(tokenizer.hasMoreTokens) {
        val t = tokenizer.nextToken
        token += t
        if (t == "'") return
      }
      case "\"" => while (tokenizer.hasMoreTokens) {
        val t = tokenizer.nextToken
        token += t
        if (t == "\"") return
      }
      case _ =>
    }

    def out(): Unit =
      if (KEYWORDS.contains(lcToken)) {
        if (lower_?)
          result.append(lcToken)
        else result.append(lcToken.toUpperCase)
      } else result.append(token)

  }

}
