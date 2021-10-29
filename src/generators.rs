use permutation_iterator::Permutor;
use rand::{rngs::StdRng, thread_rng, Rng, SeedableRng};
use std::any::{Any, TypeId};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::ops::BitOr;
use std::rc::Rc;

type Size = u32;
type Count = u128;

pub trait Gen<T> {
    fn count(&self, factory: &mut Factory, size: Size) -> Count;
    fn get(&self, factory: &mut Factory, size: Size, index: Count) -> Option<T>;
}

pub struct Factory {
    generators: HashMap<TypeId, Box<dyn Any>>,
    counts_cache: HashMap<(TypeId, Size), Count>,
}

/***********
 * Factory *
 ***********/

impl Factory {
    pub fn new() -> Factory {
        Factory {
            generators: HashMap::new(),
            counts_cache: HashMap::new(),
        }
    }

    pub fn register<T: 'static>(&mut self, generator: impl Gen<T> + Any) {
        let id = TypeId::of::<T>();
        if self.generators.get(&id).is_some() {
            panic!(
                "Cannot register multiple generators for the same type {:?}",
                id
            );
        }

        let rc: Rc<dyn Gen<T>> = Rc::new(generator);
        self.generators.insert(id, Box::new(rc));
    }

    pub fn count<T: 'static>(&mut self, size: Size) -> Count {
        let id = TypeId::of::<T>();
        if let Some(count) = self.counts_cache.get(&(id, size)) {
            return *count;
        }

        let generator = self.lookup::<T>();
        let count = generator.count(self, size);
        self.counts_cache.insert((id, size), count);
        count
    }

    pub fn get<T: 'static>(&mut self, size: Size, index: Count) -> Option<T> {
        let generator = self.lookup::<T>();
        generator.get(self, size, index)
    }

    /// Only works if count[size] fits within u64!
    pub fn random<T: 'static>(&mut self, size: Size) -> Option<T> {
        let count = self.count::<T>(size);
        if count == 0 {
            return None;
        }
        let index = thread_rng().gen_range(0..count);
        self.get(size, index)
    }

    /// Only works if count[size] fits within u64!
    pub fn random_with_seed<T: 'static>(&mut self, size: Size, seed: [u8; 32]) -> Option<T> {
        let count = self.count::<T>(size);
        if count == 0 {
            return None;
        }
        let index = StdRng::from_seed(seed).gen_range(0..count);
        self.get(size, index)
    }

    pub fn iter<T: 'static>(&mut self, size: Size) -> impl Iterator<Item = T> + '_ {
        Iter::new(self, size)
    }

    pub fn iter_all<T: 'static>(&mut self) -> impl Iterator<Item = T> + '_ {
        IterAll::new(self)
    }

    pub fn iter_random<T: 'static>(&mut self, size: Size) -> impl Iterator<Item = T> + '_ {
        IterRandom::new(self, size)
    }

    pub fn iter_random_with_seed<T: 'static>(
        &mut self,
        size: Size,
        seed: [u8; 32],
    ) -> impl Iterator<Item = T> + '_ {
        IterRandom::from_seed(self, size, seed)
    }

    fn lookup<T: 'static>(&mut self) -> Rc<dyn Gen<T>> {
        let id = TypeId::of::<T>();
        if let Some(generator) = self.generators.get(&id) {
            generator
                .as_ref()
                .downcast_ref::<Rc<dyn Gen<T>>>()
                .unwrap()
                .clone()
        } else {
            panic!("Generator not registered for type {:?}", id);
        }
    }
}

/****************
 * Constructors *
 ****************/

pub fn gen_inc<A, G: Gen<A>>(generator: G) -> impl Gen<A> {
    GenInc {
        generator,
        phantom: PhantomData,
    }
}

pub fn gen_rec<A: 'static>() -> impl Gen<A> {
    GenRec(PhantomData)
}

pub fn gen_value<T: Clone>(value: T) -> impl Gen<T> {
    GenValue(value)
}

pub fn gen_set<T: Clone>(values: Vec<T>) -> impl Gen<T> {
    GenSet(values)
}

pub fn gen_pair<A, B, GA: Gen<A>, GB: Gen<B>>(left: GA, right: GB) -> impl Gen<(A, B)> {
    GenPair {
        left,
        right,
        phantom_a: PhantomData,
        phantom_b: PhantomData,
    }
}

pub fn gen_choice<A, G1: Gen<A>, G2: Gen<A>>(left: G1, right: G2) -> impl Gen<A> {
    GenChoice {
        left,
        right,
        phantom: PhantomData,
    }
}

pub fn gen_map<A, B, GA: Gen<A>, F: Fn(A) -> B>(generator: GA, func: F) -> impl Gen<B> {
    GenMap {
        generator,
        func,
        phantom: PhantomData,
    }
}

/*
impl<G1: Gen<T>, T> BitOr for G1 {
    type Output = GenChoice<A, G1, G2>;

    fn bitor(self, other: G2) -> GenChoice<A, G1, G2> {
        GenChoice {
            left: self,
            right: other,
            phantom: PhantomData,
        }
    }
}
*/

/*************
 * Recursion *
 *************/

struct GenRec<T>(PhantomData<T>);

impl<T: 'static> Gen<T> for GenRec<T> {
    fn count(&self, factory: &mut Factory, size: Size) -> Count {
        factory.count::<T>(size)
    }

    fn get(&self, factory: &mut Factory, size: Size, index: Count) -> Option<T> {
        factory.get::<T>(size, index)
    }
}

struct GenInc<T, G: Gen<T>> {
    generator: G,
    phantom: PhantomData<T>,
}

impl<T, G: Gen<T>> Gen<T> for GenInc<T, G> {
    fn count(&self, factory: &mut Factory, size: Size) -> Count {
        if size == 0 {
            0
        } else {
            self.generator.count(factory, size - 1)
        }
    }

    fn get(&self, factory: &mut Factory, size: Size, index: Count) -> Option<T> {
        if size == 0 {
            None
        } else {
            self.generator.get(factory, size - 1, index)
        }
    }
}

/**********
 * Values *
 **********/

struct GenValue<T: Clone>(T);

impl<T: Clone> Gen<T> for GenValue<T> {
    fn count(&self, _factory: &mut Factory, size: Size) -> Count {
        if size == 0 {
            1
        } else {
            0
        }
    }

    fn get(&self, _factory: &mut Factory, size: Size, index: Count) -> Option<T> {
        if size == 0 && index == 0 {
            Some(self.0.clone())
        } else {
            None
        }
    }
}

struct GenSet<T: Clone>(Vec<T>);

impl<T: Clone> Gen<T> for GenSet<T> {
    fn count(&self, _factory: &mut Factory, size: Size) -> Count {
        if size == 0 {
            self.0.len() as Count
        } else {
            0
        }
    }

    fn get(&self, _factory: &mut Factory, size: Size, index: Count) -> Option<T> {
        if size == 0 {
            self.0.get(index as usize).cloned()
        } else {
            None
        }
    }
}

/*********
 * Pairs *
 *********/

struct GenPair<A, B, GA: Gen<A>, GB: Gen<B>> {
    left: GA,
    right: GB,
    phantom_a: PhantomData<A>,
    phantom_b: PhantomData<B>,
}

impl<A, B, GA: Gen<A>, GB: Gen<B>> Gen<(A, B)> for GenPair<A, B, GA, GB> {
    fn count(&self, factory: &mut Factory, size: Size) -> Count {
        let mut count = 0;
        for lsize in 0..=size {
            let rsize = size - lsize;
            let lcount = self.left.count(factory, lsize);
            let rcount = self.right.count(factory, rsize);
            count += lcount * rcount;
        }
        count
    }

    fn get(&self, factory: &mut Factory, size: Size, mut index: Count) -> Option<(A, B)> {
        for lsize in 0..=size {
            let rsize = size - lsize;
            let lcount = self.left.count(factory, lsize);
            let rcount = self.right.count(factory, rsize);
            let count = lcount * rcount;
            if index < count {
                let lindex = index / rcount;
                let rindex = index % rcount;
                let lval = self.left.get(factory, lsize, lindex).unwrap();
                let rval = self.right.get(factory, rsize, rindex).unwrap();
                return Some((lval, rval));
            }
            index -= count;
        }
        None
    }
}

/*************
 * Sequences *
 *************/

struct GenSeq<A, F: Fn(Size) -> A> {
    func: F,
    phantom: PhantomData<A>,
}

impl<A, F: Fn(Size) -> A> Gen<A> for GenSeq<A, F> {
    fn count(&self, factory: &mut Factory, size: Size) -> Count {
        1
    }

    fn get(&self, factory: &mut Factory, size: Size, index: Count) -> Option<A> {
        if index == 0 {
            None
        } else {
            Some((self.func)(size))
        }
    }
}

/***********
 * Mapping *
 ***********/

struct GenMap<A, B, GA: Gen<A>, F: Fn(A) -> B> {
    generator: GA,
    func: F,
    phantom: PhantomData<A>,
}

impl<A, B, GA: Gen<A>, F: Fn(A) -> B> Gen<B> for GenMap<A, B, GA, F> {
    fn count(&self, factory: &mut Factory, size: Size) -> Count {
        self.generator.count(factory, size)
    }

    fn get(&self, factory: &mut Factory, size: Size, index: Count) -> Option<B> {
        self.generator.get(factory, size, index).map(&self.func)
    }
}

/***********
 * Choice *
 ***********/

struct GenChoice<A, G1: Gen<A>, G2: Gen<A>> {
    left: G1,
    right: G2,
    phantom: PhantomData<A>,
}

impl<A, G1: Gen<A>, G2: Gen<A>> Gen<A> for GenChoice<A, G1, G2> {
    fn count(&self, factory: &mut Factory, size: Size) -> Count {
        self.left.count(factory, size) + self.right.count(factory, size)
    }

    fn get(&self, factory: &mut Factory, size: Size, index: Count) -> Option<A> {
        let left_count = self.left.count(factory, size);
        if index < left_count {
            self.left.get(factory, size, index)
        } else {
            self.right.get(factory, size, index - left_count)
        }
    }
}

/*************
 * Iterators *
 *************/

struct IterAll<'a, T> {
    factory: &'a mut Factory,
    size: Size,
    index: Count,
    count: Count,
    phantom: PhantomData<T>,
}

impl<'a, T: 'static> IterAll<'a, T> {
    fn new(factory: &'a mut Factory) -> IterAll<'a, T> {
        let count = factory.count::<T>(0);
        IterAll {
            factory,
            size: 0,
            index: 0,
            count,
            phantom: PhantomData,
        }
    }
}

impl<'a, T: 'static> Iterator for IterAll<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        while self.index >= self.count {
            self.size += 1;
            self.index = 0;
            self.count = self.factory.count::<T>(self.size);
        }
        let item = self.factory.get::<T>(self.size, self.index).unwrap();
        self.index += 1;
        return Some(item);
    }
}

struct Iter<'a, T> {
    factory: &'a mut Factory,
    size: Size,
    index: Count,
    count: Count,
    phantom: PhantomData<T>,
}

impl<'a, T: 'static> Iter<'a, T> {
    fn new(factory: &'a mut Factory, size: Size) -> Iter<'a, T> {
        let count = factory.count::<T>(size);
        Iter {
            factory,
            size,
            index: 0,
            count,
            phantom: PhantomData,
        }
    }
}

impl<'a, T: 'static> Iterator for Iter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if self.index >= self.count {
            return None;
        }
        let item = self.factory.get::<T>(self.size, self.index).unwrap();
        self.index += 1;
        return Some(item);
    }
}

struct IterRandom<'a, T> {
    factory: &'a mut Factory,
    permutor: Permutor,
    size: Size,
    phantom: PhantomData<T>,
}

impl<'a, T: 'static> IterRandom<'a, T> {
    fn new(factory: &'a mut Factory, size: Size) -> IterRandom<'a, T> {
        let count = factory.count::<T>(size);
        let permutor = Permutor::new(count as u64);
        IterRandom {
            factory,
            permutor,
            size,
            phantom: PhantomData,
        }
    }

    fn from_seed(factory: &'a mut Factory, size: Size, seed: [u8; 32]) -> IterRandom<'a, T> {
        let count = factory.count::<T>(size);
        let permutor = Permutor::new_with_slice_key(count as u64, seed);
        IterRandom {
            factory,
            permutor,
            size,
            phantom: PhantomData,
        }
    }
}

impl<'a, T: 'static> Iterator for IterRandom<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        if let Some(index) = self.permutor.next() {
            self.factory.get::<T>(self.size, index as Count)
        } else {
            None
        }
    }
}

/***********
 * Testing *
 ***********/

#[cfg(test)]
mod testing {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq)]
    enum Tree {
        L,
        B(Box<Tree>, Box<Tree>),
    }

    impl Tree {
        fn size(&self) -> Size {
            match self {
                Tree::L => 0,
                Tree::B(x, y) => 1 + x.size() + y.size(),
            }
        }
    }

    fn leaf() -> Tree {
        Tree::L
    }

    fn branch(left: Tree, right: Tree) -> Tree {
        Tree::B(Box::new(left), Box::new(right))
    }

    fn tree_factory() -> Factory {
        let mut factory = Factory::new();

        let leaf_gen = gen_value(Tree::L);
        let branch_gen = gen_inc(gen_map(gen_pair(gen_rec(), gen_rec()), |(a, b)| {
            Tree::B(Box::new(a), Box::new(b))
        }));
        let tree_gen = gen_choice(leaf_gen, branch_gen);
        factory.register(tree_gen);
        factory
    }

    #[test]
    fn test_count_and_get() {
        let mut factory: Factory = tree_factory();

        assert_eq!(factory.count::<Tree>(0), 1);
        assert_eq!(factory.count::<Tree>(1), 1);
        assert_eq!(factory.count::<Tree>(2), 2);
        assert_eq!(factory.count::<Tree>(3), 5);
        assert_eq!(factory.count::<Tree>(4), 14);

        assert_eq!(
            factory.get::<Tree>(3, 4),
            Some(branch(branch(branch(leaf(), leaf()), leaf()), leaf()))
        );
    }

    #[test]
    fn test_random() {
        let mut factory: Factory = tree_factory();

        let tree = factory.random::<Tree>(10).unwrap();
        assert_eq!(tree.size(), 10);

        let tree = factory.random_with_seed::<Tree>(10, [0; 32]).unwrap();
        assert_eq!(tree.size(), 10);
    }

    #[test]
    fn test_iter() {
        let mut factory: Factory = tree_factory();

        {
            let iter = factory.iter_all::<Tree>();
            // drop the one tree of size 0, and the one tree of size 1
            let mut iter = iter.skip(2);
            assert_eq!(iter.next().unwrap().size(), 2);
            assert_eq!(iter.next().unwrap().size(), 2);
            assert_eq!(iter.next().unwrap().size(), 3);
        }

        {
            let iter = factory.iter::<Tree>(2);
            assert_eq!(iter.count(), 2);
        }

        {
            let iter = factory.iter_random::<Tree>(4);
            assert_eq!(iter.count(), 14);
        }

        {
            let iter = factory.iter_random_with_seed::<Tree>(4, [0; 32]);
            assert_eq!(iter.count(), 14);
        }
    }
}
