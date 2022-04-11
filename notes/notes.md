## Our Design

Why choice? For synchronizing choices, e.g. "all one one line, or all on separate lines".

## Runtime

TODO: Need to pin down what `n` is in our big-O calculations. Maybe it's `max(DAG-size,
output-size)`? Or maybe it's DAG-size, and we have this restriction? `size(a + b) = size(a) + size(b)`

Runtime for Bernardy-style printers is based on shape:

- Rectangles is O(nw), where n is the size of the doc and w is the max width
- "Paragraph" shapes is O(nw^2)
- Shapes with first,middle,last is O(nw^3)

According to this, Bernardy _should_ be O(nw^2), but is actually O(nw^4). This is because he keeps
sets of size O(nw^2), and has to combine them.

[TODO: mixing up runtime and set size above. Clear this up]

Runtime for _our_ printer is O(n^k)! Trouble is, there's no guarantee you'll ever see a newline :-).

### Wadler Runtime

The worst case runtime of Wadler's printer is `O(nw)`, `O(n^2)`, or `O(2^n)`, depending on how
unfair you want to be. Let's discuss these in reverse order.

For details, see `prettier.hs` and `run_prettier.sh`.

Strictly speaking, using the measurement by which our printer runs in worst case time `O(nw^k)`
[FILL k], Wadler's printer runs in worst case time `O(2^n)`. Here is a document whose output size is
0, whose DAG size is `n`, and whose runtime is `O(2^n)`:

    -- Runtime exponential in n, when printed at any size
    huge :: Int -> DOC
    huge 0 = nil
    huge n = let d = huge (n - 1) in d <> d

(`<>` means concat.) This is very unfair, though. It exploits the fact that Wadler's printer expands
all of the concatenations, under the very reasonable assumption that those concatenations will
contain content.

Our next most unfair document does contain content, but all of that content comes _after_ groups.
This forces his printer to open `n` groups before it gets to see the first piece of content (a
newline):

    -- Runtime quadratic in n, when printed at any size
    antagonistic :: Int -> DOC
    antagonistic 0 = text "line"
    antagonistic n = group (antagonistic (n - 1) <> line <> text "line")

This is perhaps an unrealistically antagonistic document: it would be reasonable to assume that most
groups contained some content _preceding_ any nested group.

Under that assumption, Wadler's printer runs in `O(nw)` time:

    -- Runtime quadratic in n, when printed at size n
    nestedLists :: Int -> DOC
    nestedLists 0 = text "[]"
    nestedLists n = group (text "[" <> (nestedLists (n - 1)) <> text "]")

Why do I say `O(nw)` when the above example is quadratic? Say `n = kw`. Then you can construct `k`
nested lists, each with runtime quadratic in `w`, giving a total runtime of `O(kw^2) = O(nw)`.

### Reasonable-ness assumption

Some of the above examples are slow, but seem like _unreasonable_ inputs. What counts as reasonable,
though? One suggestion by Kiselyov, Peyton-Jones, and Sabry in "Lazy v. Yield: Incremental, Linear
Pretty-printing":

> The function genB performs normalization of the document, ensuring that: (i) every group is
> strictly wider than any of its children groups (thus eliminating Group (Group ...)); (ii) any
> group is at least one-character wide. Normalization is often overlooked, yet critical: without it
> no pretty-printing algorithm can have bounded look-ahead. For example, in a document with a branch
> (Group ◦ Group ◦ ... ◦ Group) (Text ””) 19 with an arbitrarily long sequence of Groups, any
> pretty-printing algorithm without normalization has to scan the whole branch, which can be
> arbitrarily long, to determine that it does not contribute to the width of the current branch.

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
tree-shaped docs. (Well, it's better in the case it doesn't explode exponentially.) A bad case, that
has to explode exponentially to get into normal form:

    a = "A" + b + "A" | b
    b = "B" + c + "B" | c
    c = "C" + d + "C" | d
    d = "D" | empty

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

## Example that shows that Swierstra is slow

    import UU_Pretty
    import UU_Pretty_ext
    
    p n a = ((par >>|<< (c2e (text (show n)))) >>^<< (par >>-<< (c2e (text (show n))))) >>$<< [a]
    
    loop n =
      if n == 0 then c2e (text "") else p n (loop (n - 1))
    
    main = do
      render (((loop 100) >>|<< par) >>$< [text ""]) 80

