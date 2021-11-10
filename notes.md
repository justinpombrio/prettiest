## Runtime

Runtime for Bernardy-style printers is based on shape:

- Rectangles is O(nw), where n is the size of the doc and w is the max width
- "Paragraph" shapes is O(nw^2)
- Shapes with first,middle,last is O(nw^3)

According to this, Bernardy _should_ be O(nw^2), but is actually O(nw^4). This is because he keeps
sets of size O(nw^2), and has to combine them.

[TODO: mixing up runtime and set size above. Clear this up]

Wadler runtime is more complex [FILL].

## Trees or DAGs?

Is a Doc a tree or a DAG? DAGs are more expressive, but you have to be careful with them. If you
process them in any way it's easy to accidentally turn them into a tree.

If a Doc is a Tree, then I think we can use Yelland-style Doc normalization to shave off a factor of
O(w), while at most doubling (1.5xing?) the size of a Doc.

Yelland-style normal form is for docs of shape:

    e ::= t | e + e | e ^ e | (e | e)

The normal form is that in `e1 + e2`, `e1` always has the form `t`. This can be achieved by rewrite
rules:

    (a + b) + c -> a + (b + c)
    (a ^ b) + c -> a ^ (b + c)
    (a | b) + c -> a + c | b + c

Yelland (if I understood correctly) applied these rewrite rules to DAGs. But they can expand a DAG
exponenentially, so I don't think there's any point to applying it to DAG-shaped docs instead of
tree-shaped docs. (Well, it's better in the case it doesn't explode exponentially.) A bad case:

    a = b | b + "a"
    b = c | c + "b"
    c = d | d + "c"
    d = "d"

This run in exponential time for Yelland. And also for Wadler:

    isItSlow :: Int -> DOC
    isItSlow 0 = text "a" :<|> text ""
    isItSlow n = (doc <> text [chr ((n `mod` 26) + 97)]) :<|> doc
      where doc = isItSlow (n - 1)

Note that this is exponential time with respect to the size of the Doc DAG, and exponential with
respect to the maximum size of the output of the printer, but polynomial with respect to the size of
the Doc as a Tree.

## Complexity

Pretty printing is NP-complete. You can see this from a reduction from the knapsack problem:

- knapsack weight limit -> pretty printing width limit
- knapsack items -> horz-cat of a sequence of choices
- a knapsack item with weight w and value V - v ->
  "aaaaa..."(length=w) | "\n\n\n..."(length=v)
(where V is some bigish constant that's larger than any value)

For example, the knapsack problem:
{(weight=2,value=1), (weight=3,value=2), (weight=4, value=1)}
with V=3 turns into the Doc:
("aa" | NL + NL) + ("bbb" | NL) + ("cccc" | NL + NL)
