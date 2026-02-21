using System;
using System.Collections.Generic;
using System.Globalization;

namespace DesignPatternChallenge
{
    // ============================
    // CONTEXTO
    // ============================
    public class ShoppingCart
    {
        public decimal TotalValue { get; set; }
        public int ItemQuantity { get; set; }
        public string CustomerCategory { get; set; } = "Regular";
        public bool IsFirstPurchase { get; set; }
    }

    // ============================
    // TOKENIZER
    // ============================
    public enum TokenType
    {
        Identifier,
        Number,
        String,
        Bool,

        Greater, GreaterEqual,
        Less, LessEqual,
        Equal,

        And, Or, Not,
        LParen, RParen,
        Then,  // ENTAO / ENTÃO
        End
    }

    public readonly struct Token
    {
        public TokenType Type { get; }
        public string Lexeme { get; }

        public Token(TokenType type, string lexeme)
        {
            Type = type;
            Lexeme = lexeme;
        }

        public override string ToString() => $"{Type}('{Lexeme}')";
    }

    public class Tokenizer
    {
        private readonly string _text;
        private int _pos;

        public Tokenizer(string text)
        {
            _text = text ?? "";
            _pos = 0;
        }

        public List<Token> Tokenize()
        {
            var tokens = new List<Token>();

            while (!IsAtEnd())
            {
                SkipWhite();
                if (IsAtEnd()) break;

                char c = Peek();

                if (c == '(') { Advance(); tokens.Add(new Token(TokenType.LParen, "(")); continue; }
                if (c == ')') { Advance(); tokens.Add(new Token(TokenType.RParen, ")")); continue; }

                if (c == '>')
                {
                    Advance();
                    if (!IsAtEnd() && Peek() == '=')
                    {
                        Advance();
                        tokens.Add(new Token(TokenType.GreaterEqual, ">="));
                    }
                    else tokens.Add(new Token(TokenType.Greater, ">"));
                    continue;
                }

                if (c == '<')
                {
                    Advance();
                    if (!IsAtEnd() && Peek() == '=')
                    {
                        Advance();
                        tokens.Add(new Token(TokenType.LessEqual, "<="));
                    }
                    else tokens.Add(new Token(TokenType.Less, "<"));
                    continue;
                }

                if (c == '=')
                {
                    Advance();
                    tokens.Add(new Token(TokenType.Equal, "="));
                    continue;
                }

                // String entre aspas: "VIP"
                if (c == '"')
                {
                    Advance(); // abre "
                    int start = _pos;
                    while (!IsAtEnd() && Peek() != '"') Advance();
                    string str = _text.Substring(start, _pos - start);
                    if (!IsAtEnd()) Advance(); // fecha "
                    tokens.Add(new Token(TokenType.String, str));
                    continue;
                }

                // Número (aceita decimal com .)
                if (char.IsDigit(c))
                {
                    int start = _pos;
                    while (!IsAtEnd() && (char.IsDigit(Peek()) || Peek() == '.')) Advance();
                    string num = _text.Substring(start, _pos - start);
                    tokens.Add(new Token(TokenType.Number, num));
                    continue;
                }

                // Palavra (identificador ou keyword)
                if (char.IsLetter(c) || c == '_')
                {
                    int start = _pos;
                    while (!IsAtEnd() && (char.IsLetterOrDigit(Peek()) || Peek() == '_' || Peek() == 'ç' || Peek() == 'ã' || Peek() == 'õ' || Peek() == 'á' || Peek() == 'é' || Peek() == 'í' || Peek() == 'ó' || Peek() == 'ú'))
                        Advance();

                    string word = _text.Substring(start, _pos - start);
                    string upper = word.ToUpperInvariant();

                    if (upper == "E") tokens.Add(new Token(TokenType.And, word));
                    else if (upper == "OU") tokens.Add(new Token(TokenType.Or, word));
                    else if (upper == "NAO" || upper == "NÃO") tokens.Add(new Token(TokenType.Not, word));
                    else if (upper == "ENTAO" || upper == "ENTÃO") tokens.Add(new Token(TokenType.Then, word));
                    else if (upper == "TRUE" || upper == "FALSE") tokens.Add(new Token(TokenType.Bool, upper.ToLowerInvariant()));
                    else tokens.Add(new Token(TokenType.Identifier, word));

                    continue;
                }

                // Ignora % e outros símbolos simples após desconto (ex: "15%")
                if (c == '%')
                {
                    Advance();
                    continue;
                }

                throw new Exception($"Caractere inválido na regra: '{c}'");
            }

            tokens.Add(new Token(TokenType.End, ""));
            return tokens;
        }

        private bool IsAtEnd() => _pos >= _text.Length;
        private char Peek() => _text[_pos];
        private void Advance() => _pos++;
        private void SkipWhite()
        {
            while (!IsAtEnd() && char.IsWhiteSpace(Peek())) Advance();
        }
    }

    // ============================
    // INTERPRETER (AST)
    // ============================
    public abstract class BoolExpression
    {
        public abstract bool Evaluate(ShoppingCart cart);
    }

    public class AndExpression : BoolExpression
    {
        private readonly BoolExpression _left, _right;
        public AndExpression(BoolExpression left, BoolExpression right) { _left = left; _right = right; }
        public override bool Evaluate(ShoppingCart cart) => _left.Evaluate(cart) && _right.Evaluate(cart);
    }

    public class OrExpression : BoolExpression
    {
        private readonly BoolExpression _left, _right;
        public OrExpression(BoolExpression left, BoolExpression right) { _left = left; _right = right; }
        public override bool Evaluate(ShoppingCart cart) => _left.Evaluate(cart) || _right.Evaluate(cart);
    }

    public class NotExpression : BoolExpression
    {
        private readonly BoolExpression _inner;
        public NotExpression(BoolExpression inner) { _inner = inner; }
        public override bool Evaluate(ShoppingCart cart) => !_inner.Evaluate(cart);
    }

    public enum CompareOp { Greater, GreaterEqual, Less, LessEqual, Equal }

    public class ComparisonExpression : BoolExpression
    {
        private readonly string _identifier;
        private readonly CompareOp _op;
        private readonly object _value;

        public ComparisonExpression(string identifier, CompareOp op, object value)
        {
            _identifier = identifier;
            _op = op;
            _value = value;
        }

        public override bool Evaluate(ShoppingCart cart)
        {
            object left = Resolve(cart, _identifier);
            return Compare(left, _op, _value);
        }

        private static object Resolve(ShoppingCart cart, string id)
        {
            string key = id.Trim().ToLowerInvariant();

            return key switch
            {
                "quantidade" => cart.ItemQuantity,
                "valor" => cart.TotalValue,
                "categoria" => cart.CustomerCategory,
                "primeiracompra" => cart.IsFirstPurchase,
                _ => throw new Exception($"Identificador desconhecido: {id}")
            };
        }

        private static bool Compare(object left, CompareOp op, object right)
        {
            // int/decimal
            if (left is int li && right is decimal rdA) return CompareDecimal(li, rdA, op);
            if (left is int li2 && right is int ri2) return CompareDecimal(li2, ri2, op);
            if (left is decimal ld && right is decimal rd) return CompareDecimal(ld, rd, op);

            // string
            if (left is string ls && right is string rs)
            {
                if (op != CompareOp.Equal) throw new Exception("String só suporta '='");
                return string.Equals(ls, rs, StringComparison.OrdinalIgnoreCase);
            }

            // bool
            if (left is bool lb && right is bool rb)
            {
                if (op != CompareOp.Equal) throw new Exception("Bool só suporta '='");
                return lb == rb;
            }

            throw new Exception($"Comparação inválida: {left} ({left.GetType().Name}) com {right} ({right.GetType().Name})");
        }

        private static bool CompareDecimal(decimal left, decimal right, CompareOp op)
        {
            return op switch
            {
                CompareOp.Greater => left > right,
                CompareOp.GreaterEqual => left >= right,
                CompareOp.Less => left < right,
                CompareOp.LessEqual => left <= right,
                CompareOp.Equal => left == right,
                _ => false
            };
        }
    }

    // ============================
    // PARSER (precedência: NOT > AND > OR)
    // Regra: <expr> ENTAO <numero>
    // expr := or
    // or   := and (OU and)*
    // and  := unary (E unary)*
    // unary:= (NAO unary) | primary
    // primary := '(' expr ')' | comparison
    // comparison := IDENT OP literal
    // literal := NUMBER | STRING | BOOL | IDENT (string sem aspas)
    // ============================
    public class Parser
    {
        private readonly List<Token> _tokens;
        private int _current;

        public Parser(List<Token> tokens)
        {
            _tokens = tokens;
            _current = 0;
        }

        public (BoolExpression condition, decimal discountPercent) ParseRule()
        {
            BoolExpression condition = ParseExpression();

            Consume(TokenType.Then, "Esperado 'ENTAO' na regra.");

            Token discountToken = Consume(TokenType.Number, "Esperado número do desconto após ENTAO.");
            decimal discount = decimal.Parse(discountToken.Lexeme, CultureInfo.InvariantCulture);

            Consume(TokenType.End, "Texto extra após o desconto.");
            return (condition, discount);
        }

        private BoolExpression ParseExpression() => ParseOr();

        private BoolExpression ParseOr()
        {
            BoolExpression expr = ParseAnd();
            while (Match(TokenType.Or))
            {
                BoolExpression right = ParseAnd();
                expr = new OrExpression(expr, right);
            }
            return expr;
        }

        private BoolExpression ParseAnd()
        {
            BoolExpression expr = ParseUnary();
            while (Match(TokenType.And))
            {
                BoolExpression right = ParseUnary();
                expr = new AndExpression(expr, right);
            }
            return expr;
        }

        private BoolExpression ParseUnary()
        {
            if (Match(TokenType.Not))
                return new NotExpression(ParseUnary());

            return ParsePrimary();
        }

        private BoolExpression ParsePrimary()
        {
            if (Match(TokenType.LParen))
            {
                BoolExpression expr = ParseExpression();
                Consume(TokenType.RParen, "Esperado ')'");
                return expr;
            }

            return ParseComparison();
        }

        private BoolExpression ParseComparison()
        {
            Token id = Consume(TokenType.Identifier, "Esperado identificador (valor, quantidade, categoria, primeiraCompra).");

            CompareOp op;
            if (Match(TokenType.Greater)) op = CompareOp.Greater;
            else if (Match(TokenType.GreaterEqual)) op = CompareOp.GreaterEqual;
            else if (Match(TokenType.Less)) op = CompareOp.Less;
            else if (Match(TokenType.LessEqual)) op = CompareOp.LessEqual;
            else if (Match(TokenType.Equal)) op = CompareOp.Equal;
            else throw new Exception("Esperado operador: >, >=, <, <=, =");

            object literal = ParseLiteral();
            return new ComparisonExpression(id.Lexeme, op, literal);
        }

        private object ParseLiteral()
        {
            if (Match(TokenType.Number))
                return decimal.Parse(Previous().Lexeme, CultureInfo.InvariantCulture);

            if (Match(TokenType.String))
                return Previous().Lexeme;

            if (Match(TokenType.Bool))
                return Previous().Lexeme == "true";

            // permite: categoria = VIP (VIP sem aspas)
            if (Match(TokenType.Identifier))
                return Previous().Lexeme;

            throw new Exception("Esperado literal (número, string ou bool).");
        }

        // Helpers
        private bool Match(TokenType type)
        {
            if (Check(type))
            {
                Advance();
                return true;
            }
            return false;
        }

        private bool Check(TokenType type) => Peek().Type == type;
        private Token Peek() => _tokens[_current];
        private Token Previous() => _tokens[_current - 1];

        private Token Advance()
        {
            if (_tokens[_current].Type != TokenType.End) _current++;
            return Previous();
        }

        private Token Consume(TokenType type, string message)
        {
            if (Check(type)) return Advance();
            throw new Exception($"{message} (Token atual: {Peek()})");
        }
    }

    // ============================
    // ENGINE (aplica regras)
    // ============================
    public class DiscountRuleEngine
    {
        public decimal EvaluateDiscountPercent(ShoppingCart cart, string ruleText)
        {
            Console.WriteLine($"Avaliando regra: {ruleText}");

            var tokens = new Tokenizer(ruleText).Tokenize();
            var (condition, discount) = new Parser(tokens).ParseRule();

            bool ok = condition.Evaluate(cart);

            if (ok)
            {
                Console.WriteLine($"✓ Regra aplicada: {discount}% desconto");
                return discount;
            }

            Console.WriteLine("✗ Regra não aplicável");
            return 0;
        }

        public decimal BestDiscountPercent(ShoppingCart cart, IEnumerable<string> rules)
        {
            decimal best = 0;

            foreach (var rule in rules)
            {
                try
                {
                    var d = EvaluateDiscountPercent(cart, rule);
                    if (d > best) best = d;
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"⚠️ Regra inválida: {ex.Message}");
                }

                Console.WriteLine();
            }

            return best;
        }
    }

    // ============================
    // DEMO
    // ============================
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("=== Sistema de Regras de Desconto (Interpreter) ===\n");

            var engine = new DiscountRuleEngine();

            var cart1 = new ShoppingCart
            {
                TotalValue = 1500.00m,
                ItemQuantity = 15,
                CustomerCategory = "Regular",
                IsFirstPurchase = false
            };

            var cart2 = new ShoppingCart
            {
                TotalValue = 500.00m,
                ItemQuantity = 5,
                CustomerCategory = "VIP",
                IsFirstPurchase = false
            };

            var cart3 = new ShoppingCart
            {
                TotalValue = 200.00m,
                ItemQuantity = 2,
                CustomerCategory = "Regular",
                IsFirstPurchase = true
            };

            // Regras (como se viessem do banco)
            var rules = new List<string>
            {
                "quantidade > 10 E valor > 1000 ENTAO 15",
                "categoria = VIP ENTAO 20",
                "primeiraCompra = true ENTAO 10",

                // regras complexas (agora suportadas!)
                "(quantidade > 10 OU valor > 500) E categoria = VIP ENTAO 25",
                "NAO primeiraCompra E quantidade >= 5 ENTAO 5",
                "(valor > 1000 E categoria = VIP) OU primeiraCompra = true ENTAO 12"
            };

            Console.WriteLine("=== Carrinho 1 ===\n");
            var best1 = engine.BestDiscountPercent(cart1, rules);
            Console.WriteLine($"Melhor desconto carrinho 1: {best1}%\n");

            Console.WriteLine("=== Carrinho 2 ===\n");
            var best2 = engine.BestDiscountPercent(cart2, rules);
            Console.WriteLine($"Melhor desconto carrinho 2: {best2}%\n");

            Console.WriteLine("=== Carrinho 3 ===\n");
            var best3 = engine.BestDiscountPercent(cart3, rules);
            Console.WriteLine($"Melhor desconto carrinho 3: {best3}%\n");
        }
    }
}