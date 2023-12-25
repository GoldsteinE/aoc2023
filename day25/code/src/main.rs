use std::{
    collections::VecDeque,
    fmt, io, mem,
    str::FromStr,
    sync::atomic::{self, AtomicUsize},
    thread,
    time::{Duration, Instant},
};

use color_eyre::eyre::{self, bail, WrapErr as _};
use rand::{Rng as _, SeedableRng as _};
use rayon::prelude::*;

use ahash::{AHashMap as HashMap, AHashSet as HashSet};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Node {
    name: [u8; 3],
}

impl FromStr for Node {
    type Err = eyre::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let &name: &[u8; 3] = s.as_bytes().try_into().wrap_err("wrong name length")?;
        Ok(Node { name })
    }
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // SAFETY: we created the node from an str.
        f.write_str(unsafe { std::str::from_utf8_unchecked(&self.name) })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Edge(Node, Node);

impl Edge {
    pub fn new(mut first: Node, mut second: Node) -> Self {
        if first > second {
            mem::swap(&mut first, &mut second);
        }
        Self(first, second)
    }
}

#[derive(Clone, Debug, Default)]
struct Graph {
    nodes: HashSet<Node>,
    edges: HashMap<Edge, Vec<Edge>>,
    // Important for fast random choice.
    edges_vec: Vec<Edge>,
}

impl Graph {
    // That’s Karger's algorithm. It’s probabilistic, so it accepts a seed.
    //
    // Note that Karger-Stein algorithm requires cloning the graph, so is in fact slower
    // in my benchmarks, despite being algorithmically better.
    fn contract(&mut self, seed: u64) {
        let mut rng = rand_xoshiro::Xoshiro128StarStar::seed_from_u64(seed);
        let mut removed_edges = Vec::new();
        while self.nodes.len() > 2 {
            let edge = loop {
                let edge_idx = rng.gen_range(0..self.edges_vec.len());
                let edge = self.edges_vec.swap_remove(edge_idx);
                if self.edges.contains_key(&edge) {
                    break edge;
                }
            };

            let (removed_node, merged_node) = if rand::random() {
                (edge.0, edge.1)
            } else {
                (edge.1, edge.0)
            };
            self.nodes.remove(&removed_node);

            // TODO: that's the hot part. Everything else is virtually free, all the time is spent
            // in this call.
            self.edges.retain(|&virt, real| {
                if virt.0 == removed_node || virt.1 == removed_node {
                    removed_edges.push((virt, mem::take(real)));
                    false
                } else {
                    true
                }
            });

            for (virt, real) in removed_edges.drain(..) {
                if virt.0 == removed_node && virt.1 == removed_node {
                    // Nothing to do, self-loops are useless.
                } else if virt.0 == removed_node {
                    if merged_node != virt.1 {
                        self.edges
                            .entry(Edge::new(merged_node, virt.1))
                            .or_insert_with_key(|&edge| {
                                self.edges_vec.push(edge);
                                <_>::default()
                            })
                            .extend_from_slice(&real);
                    }
                } else if virt.1 == removed_node {
                    if merged_node != virt.0 {
                        self.edges
                            .entry(Edge::new(virt.0, merged_node))
                            .or_insert_with_key(|&edge| {
                                self.edges_vec.push(edge);
                                <_>::default()
                            })
                            .extend_from_slice(&real);
                    }
                } else {
                    continue;
                }
            }
        }
    }
}

static CONTRACTIONS: AtomicUsize = AtomicUsize::new(0);

fn progress_watcher() {
    let mut prev_time = Instant::now();
    let mut prev_contractions = CONTRACTIONS.load(atomic::Ordering::Relaxed);
    loop {
        let now = Instant::now();
        let contractions = CONTRACTIONS.load(atomic::Ordering::Relaxed);
        let delta = contractions - prev_contractions;
        eprintln!("{delta} contractions in {:?}", now - prev_time);
        prev_time = now;
        prev_contractions = contractions;
        thread::sleep(Duration::from_secs(1));
    }
}

fn main() -> eyre::Result<()> {
    color_eyre::install()?;

    let mut graph = Graph::default();
    for line in io::stdin().lines() {
        let line = line?;
        let Some((source, raw_dests)) = line.split_once(": ") else {
            bail!("failed to parse `{line}`");
        };
        let source_node = source.parse()?;
        graph.nodes.insert(source_node);
        for dest in raw_dests.split(' ') {
            let dest_node = dest.parse()?;
            graph.nodes.insert(dest_node);
            let edge = Edge::new(source_node, dest_node);
            graph.edges.insert(edge, vec![edge]);
            graph.edges_vec.push(edge);
        }
    }

    assert_eq!(graph.edges.len(), graph.edges_vec.len());

    thread::spawn(progress_watcher);

    let to_remove = (0..u64::MAX)
        .into_par_iter()
        .find_map_any(|seed| {
            let mut g = graph.clone();
            g.contract(seed);
            CONTRACTIONS.fetch_add(1, atomic::Ordering::Relaxed);
            let contact_points = g.edges.values().next().unwrap();
            if contact_points.len() == 3 {
                return Some(contact_points.clone());
            }
            None
        })
        .unwrap();

    for edge in to_remove {
        graph.edges.remove(&edge);
    }

    // Do a simple BFS to find connected components.
    let mut adjacency: HashMap<_, Vec<_>> = HashMap::default();
    for edge in graph.edges.keys() {
        adjacency.entry(edge.0).or_default().push(edge.1);
        adjacency.entry(edge.1).or_default().push(edge.0);
    }

    let &starting_node = graph.nodes.iter().next().unwrap();
    let mut todo = VecDeque::new();
    todo.push_back(starting_node);
    let mut seen = HashSet::default();
    while let Some(node) = todo.pop_front() {
        seen.insert(node);
        if let Some(others) = adjacency.get(&node) {
            for &other in others {
                if !seen.contains(&other) {
                    todo.push_back(other);
                }
            }
        }
    }

    println!("{}", seen.len() * (graph.nodes.len() - seen.len()));

    Ok(())
}
