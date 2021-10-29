use crate::doc::{align, empty, flat, nl, spaces, text, Doc, Notation};
use crate::generators::Factory;

/*
fn doc_factory() -> Factory {
    let mut factory = Factory::new();
    factor
}

    Annotate(A, Doc<A>),
    Flat(Doc<A>),
    Align(Doc<A>),
    Concat(Doc<A>, Doc<A>),
    Choice(Doc<A>, Doc<A>),

fn gen_doc() -> Generator<Doc> {
    let gen_width: Generator<Width> = gen_seq(|n| n as Width);
    gen_recursive(|gen_doc| {
        gen_choices(vec![
            gen_value(Doc::Empty),
            gen_value(Doc::Newline),
            gen_value(Doc::EndOfLine),
            gen_sequence(|size| {
                let letter = (((size % 26) as u8) + 97) as char;
                let string = format!("{}", letter).repeat(size);
                Doc::Text(string)
            }),
            gen_width.map(|w| Spaces(w)),
            gen_pair(gen_width, gen_doc).map(|(w, d)| Indent(w, d)),


        ])
    })
}


#[derive(Debug, Clone)]
enum Tree {
    L,
    B(Box<Tree>, Box<Tree>),
}

fn tree_generator() -> Generator<Tree> {
    gen_recursive(|tree_gen| {
        let leaf_gen = gen_value(Tree::L);
        let branch_gen = gen_map(gen_pair(tree_gen.clone(), tree_gen), |(x, y)| {
            Tree::B(Box::new(x), Box::new(y))
        });
        gen_choice(leaf_gen, branch_gen)
    })
}

fn main() {
    let nats = gen_seq(|n| n);
    let pair = gen_pair(nats.clone(), nats.clone());
    let pair = gen_recursive(|_| pair.clone());
    let triple = gen_map(gen_pair(nats, pair), |(a, (b, c))| (a, b, c));
    /*
    for size in 100000..100001 {
        println!("SIZE: {}", size);
        //println!("{:?}", triple.iter(size).next().unwrap());
        //for (a, b, c) in triple.iter(size) {
        //println!("({} {} {})", a, b, c);
        //}

        println!("count: {}", triple.count(size));
    }
    */

    let tree_gen = tree_generator();
    for size in 0..5 {
        println!("SIZE: {}", size);
        for tree in tree_gen.iter(size) {
            println!("{:?}", tree);
        }
        println!("count: {}", tree_gen.count(size));
        println!();
    }
    println!("done");
}
*/
