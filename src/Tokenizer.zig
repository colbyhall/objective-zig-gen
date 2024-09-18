const Tokenizer = @This();

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const ascii = std.ascii;

pub const Token = struct {
    pub const Tag = enum {
        invalid,

        asterisk,
        ampersand,
        pound,
        bang,
        percent,
        pipe,
        question,
        tilde,

        l_paren,
        r_paren,

        l_curly,
        r_curly,

        l_bracket,
        r_bracket,

        l_arrow,
        r_arrow,
        caret,

        semicolon,
        colon,
        comma,
        period,

        equals,
        plus,
        minus,
        slash,

        eof,

        string_literal,
        char_literal,
        number_literal,

        identifier,

        keyword_interface,
        keyword_implementation,
        keyword_end,
        keyword_class,
        keyword_protocol,
        keyword_optional,
        keyword_required,
        keyword_property,
        keyword_synthesize,
        keyword_dynamic,

        keyword_unsigned,
        keyword_int,
        keyword_char,
        keyword_short,
        keyword_long,
        keyword_float,
        keyword_double,
        keyword_void,

        keyword_if,
        keyword_else,
        keyword_switch,
        keyword_case,
        keyword_default,
        keyword_for,
        keyword_while,
        keyword_do,
        keyword_break,
        keyword_continue,
        keyword_struct,
        keyword_union,
        keyword_enum,
        keyword_typedef,
        keyword_const,
        keyword_volatile,
        keyword_static,
        keyword_extern,
        keyword_return,
        keyword_goto,
        keyword_sizeof,
        keyword_auto,
        keyword_register,
    };

    pub const Loc = struct {
        start: u32,
        end: u32,
    };

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "@interface", .keyword_interface },
        .{ "@implementation", .keyword_implementation },
        .{ "@end", .keyword_end },
        .{ "@class", .keyword_class },
        .{ "@protocol", .keyword_protocol },
        .{ "@optional", .keyword_optional },
        .{ "@required", .keyword_required },
        .{ "@property", .keyword_property },
        .{ "@synthesize", .keyword_synthesize },
        .{ "@dynamic", .keyword_dynamic },
        .{ "unsigned", .keyword_unsigned },
        .{ "int", .keyword_int },
        .{ "char", .keyword_char },
        .{ "short", .keyword_short },
        .{ "long", .keyword_long },
        .{ "float", .keyword_float },
        .{ "double", .keyword_double },
        .{ "void", .keyword_void },
        .{ "if", .keyword_if },
        .{ "else", .keyword_else },
        .{ "switch", .keyword_switch },
        .{ "case", .keyword_case },
        .{ "default", .keyword_default },
        .{ "for", .keyword_for },
        .{ "while", .keyword_while },
        .{ "do", .keyword_do },
        .{ "break", .keyword_break },
        .{ "continue", .keyword_continue },
        .{ "struct", .keyword_struct },
        .{ "union", .keyword_union },
        .{ "enum", .keyword_enum },
        .{ "typedef", .keyword_typedef },
        .{ "const", .keyword_const },
        .{ "volatile", .keyword_volatile },
        .{ "static", .keyword_static },
        .{ "extern", .keyword_extern },
        .{ "return", .keyword_return },
        .{ "goto", .keyword_goto },
        .{ "sizeof", .keyword_sizeof },
        .{ "auto", .keyword_auto },
        .{ "register", .keyword_register },
    });

    tag: Tag,
    loc: Loc,
};

const State = enum {
    start,
    string_literal,
    string_literal_backslash,
    char_literal,
    char_literal_backslash,
    char_literal_end,
    number_literal,
    number_literal_period,
    identifier,
    slash,
    single_line_comment,
    multi_line_comment,
    multi_line_comment_end,
};

offset: u32,
source: [:0]const u8,

pub fn init(source: [:0]const u8) Tokenizer {
    return .{
        .offset = 0,
        .source = source,
    };
}

pub fn next(self: *Tokenizer) Token {
    var state = State.start;
    var result: Token = .{
        .tag = undefined,
        .loc = .{
            .start = self.offset,
            .end = undefined,
        },
    };
    while (true) : (self.offset += 1) {
        const c = self.source[self.offset];

        switch (state) {
            .start => switch (c) {
                0 => {
                    if (self.offset != self.source.len) {
                        result.tag = .invalid;
                        result.loc.start = self.offset;
                        self.offset += 1;
                        result.loc.end = self.offset;
                        return result;
                    }
                    result.tag = .eof;
                    break;
                },
                ' ', '\n', '\t', '\r' => {
                    result.loc.start = self.offset + 1;
                },
                '"' => {
                    state = .string_literal;
                    result.tag = .string_literal;
                },
                '\'' => {
                    state = .char_literal;
                    result.tag = .char_literal;
                },
                'a'...'z', 'A'...'Z', '_', '@' => {
                    state = .identifier;
                    result.tag = .identifier;
                },
                '0'...'9' => {
                    state = .number_literal;
                    result.tag = .number_literal;
                },
                '=' => {
                    result.tag = .equals;
                    self.offset += 1;
                    break;
                },
                '!' => {
                    result.tag = .bang;
                    self.offset += 1;
                    break;
                },
                '|' => {
                    result.tag = .pipe;
                    self.offset += 1;
                    break;
                },
                '*' => {
                    result.tag = .asterisk;
                    self.offset += 1;
                    break;
                },
                '+' => {
                    result.tag = .plus;
                    self.offset += 1;
                    break;
                },
                '-' => {
                    result.tag = .minus;
                    self.offset += 1;
                    break;
                },
                '/' => {
                    state = .slash;
                },
                '&' => {
                    result.tag = .ampersand;
                    self.offset += 1;
                    break;
                },
                '(' => {
                    result.tag = .l_paren;
                    self.offset += 1;
                    break;
                },
                ')' => {
                    result.tag = .r_paren;
                    self.offset += 1;
                    break;
                },
                '{' => {
                    result.tag = .l_curly;
                    self.offset += 1;
                    break;
                },
                '}' => {
                    result.tag = .r_curly;
                    self.offset += 1;
                    break;
                },
                '[' => {
                    result.tag = .l_bracket;
                    self.offset += 1;
                    break;
                },
                ']' => {
                    result.tag = .r_bracket;
                    self.offset += 1;
                    break;
                },
                '.' => {
                    result.tag = .period;
                    self.offset += 1;
                    break;
                },
                ',' => {
                    result.tag = .comma;
                    self.offset += 1;
                    break;
                },
                ';' => {
                    result.tag = .semicolon;
                    self.offset += 1;
                    break;
                },
                ':' => {
                    result.tag = .colon;
                    self.offset += 1;
                    break;
                },
                '<' => {
                    result.tag = .l_arrow;
                    self.offset += 1;
                    break;
                },
                '>' => {
                    result.tag = .r_arrow;
                    self.offset += 1;
                    break;
                },
                '^' => {
                    result.tag = .caret;
                    self.offset += 1;
                    break;
                },
                '%' => {
                    result.tag = .percent;
                    self.offset += 1;
                    break;
                },
                '#' => {
                    result.tag = .pound;
                    self.offset += 1;
                    break;
                },
                '?' => {
                    result.tag = .question;
                    self.offset += 1;
                    break;
                },
                '~' => {
                    result.tag = .tilde;
                    self.offset += 1;
                    break;
                },
                else => {
                    result.tag = .invalid;
                    self.offset += 1;
                    break;
                },
            },
            .identifier => switch (c) {
                'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                else => {
                    if (Token.keywords.get(self.source[result.loc.start..self.offset])) |tag| {
                        result.tag = tag;
                    }
                    break;
                },
            },
            .string_literal => switch (c) {
                '\\' => {
                    state = .string_literal_backslash;
                },
                '"' => {
                    self.offset += 1;
                    break;
                },
                0 => {
                    if (self.offset == self.source.len) {
                        result.tag = .invalid;
                        break;
                    }
                },
                '\n' => {
                    result.tag = .invalid;
                    break;
                },
                else => {},
            },
            .string_literal_backslash => switch (c) {
                0, '\n' => {
                    result.tag = .invalid;
                    break;
                },
                else => {
                    state = .string_literal;
                },
            },
            .char_literal => switch (c) {
                0, '\n', '\'' => {
                    result.tag = .invalid;
                    break;
                },
                '\\' => {
                    state = .char_literal_backslash;
                },
                else => {
                    state = .char_literal_end;
                },
            },
            .char_literal_backslash => switch (c) {
                0, '\n' => {
                    result.tag = .invalid;
                    break;
                },
                else => {
                    state = .char_literal_end;
                },
            },
            .char_literal_end => switch (c) {
                '\'' => {
                    result.tag = .char_literal;
                    self.offset += 1;
                    break;
                },
                '\\' => {
                    state = .char_literal_backslash;
                },
                else => {},
            },
            .number_literal => switch (c) {
                '.' => {
                    state = .number_literal_period;
                },
                '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {},
                else => break,
            },
            .number_literal_period => switch (c) {
                '0'...'9' => {},
                'f', 'F' => {
                    self.offset += 1;
                    break;
                },
                else => break,
            },
            .slash => switch (c) {
                '/' => {
                    state = .single_line_comment;
                },
                '*' => {
                    state = .multi_line_comment;
                },
                else => {
                    result.tag = .slash;
                    break;
                },
            },
            .single_line_comment => switch (c) {
                0 => {
                    if (self.offset != self.source.len) {
                        result.tag = .invalid;
                        self.offset += 1;
                    }
                    break;
                },
                '\n' => {
                    state = .start;
                    result.loc.start = self.offset + 1;
                },
                else => {},
            },
            .multi_line_comment => switch (c) {
                0 => {
                    if (self.offset != self.source.len) {
                        result.tag = .invalid;
                        self.offset += 1;
                    }
                    break;
                },
                '*' => {
                    state = .multi_line_comment_end;
                },
                else => {},
            },
            .multi_line_comment_end => switch (c) {
                0 => {
                    result.tag = .invalid;
                    self.offset += 1;
                    break;
                },
                '/' => {
                    state = .start;
                    result.loc.start = self.offset + 1;
                },
                else => {
                    state = .multi_line_comment;
                },
            },
        }
    }

    result.loc.end = self.offset;
    return result;
}
