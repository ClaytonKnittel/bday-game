use std::{
  collections::{HashMap, HashSet},
  fmt::{self, Debug, Formatter},
  hash::Hash,
  iter,
};

#[derive(Debug)]
pub struct ColorItem<I> {
  item: I,
  color: u32,
}

impl<I> ColorItem<I> {
  pub fn new(item: I, color: u32) -> Self {
    ColorItem { item, color }
  }
}

#[derive(Debug)]
pub enum Constraint<I> {
  Primary(I),
  Secondary(ColorItem<I>),
}

impl<I> Constraint<I> {
  fn item(&self) -> &I {
    match self {
      Constraint::Primary(item) | Constraint::Secondary(ColorItem { item, .. }) => item,
    }
  }

  fn color(&self) -> Option<u32> {
    match self {
      Constraint::Primary(_) => None,
      Constraint::Secondary(ColorItem { color, .. }) => Some(*color),
    }
  }
}

impl<I> From<I> for Constraint<I> {
  fn from(value: I) -> Self {
    Constraint::Primary(value)
  }
}

impl<I> From<ColorItem<I>> for Constraint<I> {
  fn from(value: ColorItem<I>) -> Self {
    Constraint::Secondary(value)
  }
}

struct ListNodeI<I> {
  prev: I,
  next: I,
}

type HeaderListNode = ListNodeI<u32>;

#[derive(Debug)]
pub enum HeaderType {
  Primary,
  Secondary,
}

struct Header<I> {
  item: Option<I>,
  node: HeaderListNode,
  header_type: HeaderType,
}

impl<I> Header<I> {
  fn is_primary(&self) -> bool {
    match self.header_type {
      HeaderType::Primary => true,
      HeaderType::Secondary => false,
    }
  }
}

impl<I> Debug for Header<I>
where
  I: Debug,
{
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{} (prev: {}, next: {}) ({})",
      match &self.item {
        Some(item) => format!("{item:?}"),
        None => "[None]".to_string(),
      },
      self.node.prev,
      self.node.next,
      match self.header_type {
        HeaderType::Primary => "Primary",
        HeaderType::Secondary => "Secondary",
      }
    )
  }
}

type ListNode = ListNodeI<usize>;

enum NodeType {
  Header {
    /// Number of constraints that have this item.
    size: usize,
  },
  Body {
    /// The assigned color of this node, or None if this is a primary constraint.
    color: Option<u32>,
    /// The index of the header node associated with this node.
    top: u32,
  },
}

enum Node<N> {
  Boundary {
    /// The name of the subset listed to the left of this boundary.
    name: Option<N>,
    /// The index of the first node in the subset that comes before this
    /// boundary.
    first_for_prev: usize,
    /// The index of the last node in the subset that comes after this
    /// boundary.
    last_for_next: usize,
  },
  Normal {
    /// Node in linked list of item.
    item_node: ListNode,
    node_type: NodeType,
  },
}

impl<I> Node<I> {
  fn color(&self) -> Option<u32> {
    match self {
      Node::Normal {
        node_type: NodeType::Body { color, .. },
        ..
      } => *color,
      _ => unreachable!("Unexpected color() called on non-body node"),
    }
  }

  fn color_mut(&mut self) -> &mut Option<u32> {
    match self {
      Node::Normal {
        node_type: NodeType::Body { color, .. },
        ..
      } => color,
      _ => unreachable!("Unexpected color() called on non-body node"),
    }
  }

  fn len(&self) -> usize {
    match self {
      Node::Normal {
        node_type: NodeType::Header { size },
        ..
      } => *size,
      _ => unreachable!("Node::len() called on non-header node"),
    }
  }

  fn len_mut(&mut self) -> &mut usize {
    match self {
      Node::Normal {
        node_type: NodeType::Header { size },
        ..
      } => size,
      _ => unreachable!("Node::len_mut() called on non-header node"),
    }
  }

  fn prev(&self) -> usize {
    match self {
      Node::Normal { item_node, .. } => item_node.prev,
      Node::Boundary { .. } => unreachable!("Cannot call Node::prev() on a Boundary node"),
    }
  }

  fn set_prev(&mut self, idx: usize) {
    match self {
      Node::Normal { item_node, .. } => item_node.prev = idx,
      Node::Boundary { .. } => unreachable!("Cannot call Node::set_prev() on a Boundary node"),
    }
  }

  fn next(&self) -> usize {
    match self {
      Node::Normal { item_node, .. } => item_node.next,
      Node::Boundary { .. } => unreachable!("Cannot call Node::next() on a Boundary node"),
    }
  }

  fn set_next(&mut self, idx: usize) {
    match self {
      Node::Normal { item_node, .. } => item_node.next = idx,
      Node::Boundary { .. } => unreachable!("Cannot call Node::set_next() on a Boundary node"),
    }
  }
}

impl<N> Debug for Node<N>
where
  N: Debug,
{
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Node::Boundary {
        name,
        first_for_prev,
        last_for_next,
      } => {
        write!(
          f,
          "{}: (first_prev: {}, last_next: {})",
          match name {
            Some(name) => format!("{name:?}"),
            None => "[None]".to_string(),
          },
          first_for_prev,
          last_for_next
        )
      }
      Node::Normal {
        item_node: ListNodeI { prev, next },
        node_type,
      } => {
        write!(
          f,
          "(prev: {}, next: {}) ({})",
          prev,
          next,
          match node_type {
            NodeType::Header { size } => {
              format!("Header (size: {})", size)
            }
            NodeType::Body { color, top } => {
              format!(
                "Body (top: {top}){}",
                match color {
                  Some(color) => format!(" (color: {color})"),
                  None => "".to_string(),
                }
              )
            }
          }
        )
      }
    }
  }
}

pub struct Dlx<I, N> {
  num_primary_items: usize,
  headers: Vec<Header<I>>,
  body: Vec<Node<N>>,
}

impl<I, N> Dlx<I, N>
where
  I: Hash + Eq + Clone + Debug,
  N: Hash + Eq + Clone + Debug,
{
  pub fn new<U, S, C, D>(items: U, subsets: S) -> Self
  where
    U: IntoIterator<Item = (I, HeaderType)>,
    S: IntoIterator<Item = (N, C)>,
    C: IntoIterator<Item = D>,
    D: Into<Constraint<I>>,
  {
    Self::construct(items, subsets)
  }

  fn construct<U, S, C, D>(items: U, subsets: S) -> Self
  where
    U: IntoIterator<Item = (I, HeaderType)>,
    S: IntoIterator<Item = (N, C)>,
    C: IntoIterator<Item = D>,
    D: Into<Constraint<I>>,
  {
    let mut headers = vec![Header {
      item: None,
      node: ListNodeI { prev: 0, next: 1 },
      header_type: HeaderType::Primary,
    }];
    let mut item_map = HashMap::new();
    let mut body = Vec::new();
    let mut last_start_index;
    let mut subset_names = HashSet::new();

    // Push phony node to first element of body.
    body.push(Node::Boundary {
      name: None,
      first_for_prev: 0,
      last_for_next: 0,
    });

    let (primary_headers, secondary_headers): (Vec<_>, Vec<_>) =
      items
        .into_iter()
        .partition(|(_, header_type)| match header_type {
          HeaderType::Primary => true,
          HeaderType::Secondary => false,
        });

    let primary_headers_len = primary_headers.len() as u32;
    headers.extend(
      primary_headers
        .into_iter()
        .chain(secondary_headers)
        .enumerate()
        .map(|(idx, (item, header_type))| {
          let new_idx = idx + 1;
          if item_map.insert(item.clone(), new_idx).is_some() {
            panic!("Duplicate item {:?}", item);
          }
          body.push(Node::Normal {
            item_node: ListNodeI {
              prev: new_idx,
              next: new_idx,
            },
            node_type: NodeType::Header { size: 0 },
          });

          Header {
            item: Some(item),
            node: ListNodeI {
              prev: new_idx as u32 - 1,
              next: new_idx as u32 + 1,
            },
            header_type,
          }
        }),
    );
    let last_idx = headers.len();
    headers.push(Header {
      item: None,
      node: ListNodeI {
        prev: last_idx as u32 - 1,
        next: primary_headers_len + 1,
      },
      header_type: HeaderType::Secondary,
    });
    headers.get_mut(0).unwrap().node.prev = primary_headers_len;
    headers
      .get_mut(primary_headers_len as usize)
      .unwrap()
      .node
      .next = 0;
    headers
      .get_mut(primary_headers_len as usize + 1)
      .unwrap()
      .node
      .prev = last_idx as u32;
    headers.get_mut(last_idx).unwrap().node.next = primary_headers_len + 1;

    body.push(Node::Boundary {
      name: None,
      first_for_prev: 0,
      last_for_next: 0,
    });

    for (name, constraints) in subsets {
      if !subset_names.insert(name.clone()) {
        panic!("Duplicate subset name: {name:?}");
      }

      last_start_index = body.len();
      constraints.into_iter().for_each(|constraint| {
        let constraint: Constraint<I> = constraint.into();
        let idx = body.len();

        let header_idx = *item_map
          .get(constraint.item())
          .unwrap_or_else(|| panic!("Unknown item {:?}", constraint.item()));
        let header = body.get_mut(header_idx).unwrap();
        let prev_idx = header.prev();

        debug_assert!(
          matches!(
            (headers.get(header_idx).unwrap(), &constraint),
            (
              Header {
                header_type: HeaderType::Primary,
                ..
              },
              Constraint::Primary(_),
            ) | (
              Header {
                header_type: HeaderType::Secondary,
                ..
              },
              Constraint::Secondary(_),
            )
          ),
          "Expect constraint type to match item type (primary vs. secondary)"
        );

        header.set_prev(idx);
        *header.len_mut() += 1;
        body.get_mut(prev_idx).unwrap().set_next(idx);

        body.push(Node::Normal {
          item_node: ListNodeI {
            prev: prev_idx,
            next: header_idx,
          },
          node_type: NodeType::Body {
            color: constraint.color(),
            top: header_idx as u32,
          },
        });
      });

      let last_idx = body.len() - 1;
      if let Some(Node::Boundary { last_for_next, .. }) = body.get_mut(last_start_index - 1) {
        *last_for_next = last_idx;
      } else {
        unreachable!();
      }

      body.push(Node::Boundary {
        name: Some(name),
        first_for_prev: last_start_index,
        last_for_next: 0,
      });
    }

    let num_primary_items = headers.first().unwrap().node.prev as usize;
    Dlx {
      headers,
      body,
      num_primary_items,
    }
  }

  fn header(&self, idx: usize) -> &Header<I> {
    debug_assert!((..self.headers.len()).contains(&idx));
    unsafe { self.headers.get_unchecked(idx) }
  }

  fn header_mut(&mut self, idx: usize) -> &mut Header<I> {
    debug_assert!((..self.headers.len()).contains(&idx));
    unsafe { self.headers.get_unchecked_mut(idx) }
  }

  fn body_header(&self, idx: usize) -> &Node<N> {
    debug_assert!((1..(self.headers.len() - 1)).contains(&idx));
    unsafe { self.body.get_unchecked(idx) }
  }

  fn body_header_mut(&mut self, idx: usize) -> &mut Node<N> {
    debug_assert!((1..(self.headers.len() - 1)).contains(&idx));
    unsafe { self.body.get_unchecked_mut(idx) }
  }

  fn body_node(&self, idx: usize) -> &Node<N> {
    debug_assert!(((self.headers.len() - 1)..self.body.len()).contains(&idx));
    unsafe { self.body.get_unchecked(idx) }
  }

  fn body_node_mut(&mut self, idx: usize) -> &mut Node<N> {
    debug_assert!(((self.headers.len() - 1)..self.body.len()).contains(&idx));
    unsafe { self.body.get_unchecked_mut(idx) }
  }

  fn node(&self, idx: usize) -> &Node<N> {
    debug_assert!(
      (1..(self.headers.len() - 1)).contains(&idx)
        || (self.headers.len()..self.body.len()).contains(&idx)
    );
    unsafe { self.body.get_unchecked(idx) }
  }

  fn node_mut(&mut self, idx: usize) -> &mut Node<N> {
    debug_assert!(
      (1..(self.headers.len() - 1)).contains(&idx)
        || (self.headers.len()..self.body.len()).contains(&idx)
    );
    unsafe { self.body.get_unchecked_mut(idx) }
  }

  fn item_name(&self, idx: usize) -> I {
    debug_assert!(matches!(
      self.body_node(idx),
      Node::Normal {
        node_type: NodeType::Body { .. },
        ..
      }
    ));
    if let Node::Normal {
      node_type: NodeType::Body { top, .. },
      ..
    } = self.body_node(idx)
    {
      self.header(*top as usize).item.clone().unwrap()
    } else {
      unreachable!()
    }
  }

  fn iterate_items(&self, idx: usize) -> impl Iterator<Item = usize> + '_ {
    debug_assert!(matches!(
      self.body_node(idx),
      Node::Normal {
        node_type: NodeType::Body { .. },
        ..
      }
    ));
    iter::repeat(())
      .scan(idx + 1, move |q_ptr, _| {
        let q = *q_ptr;
        if q == idx {
          return None;
        }
        match self.body_node(q) {
          Node::Boundary { first_for_prev, .. } => {
            *q_ptr = *first_for_prev;
            Some(None)
          }
          Node::Normal {
            node_type: NodeType::Body { .. },
            ..
          } => {
            *q_ptr += 1;
            Some(Some(q))
          }
          _ => None,
        }
      })
      .flatten()
  }

  /// Remove the subset containing the node at `idx` from the grid.
  fn hide(&mut self, idx: usize) {
    // println!("Hiding {idx}");
    let mut q = idx + 1;
    while q != idx {
      match self.body_node(q) {
        Node::Boundary { first_for_prev, .. } => {
          q = *first_for_prev;
        }
        Node::Normal {
          item_node,
          node_type: NodeType::Body { top, color },
        } => {
          let top = *top as usize;

          if self.header(top).is_primary() || color.is_some() {
            let prev_idx = item_node.prev;
            let next_idx = item_node.next;
            self.node_mut(prev_idx).set_next(next_idx);
            self.node_mut(next_idx).set_prev(prev_idx);
          }
          *self.body_header_mut(top).len_mut() -= 1;
          q += 1;
        }
        Node::Normal {
          node_type: NodeType::Header { .. },
          ..
        } => unreachable!("Unexpected header encountered in hide() at index {q}"),
      }
    }
  }

  /// Reverts `hide(idx)`, assuming the state of Dlx was exactly as it was when
  /// `hide(idx)` was called.
  fn unhide(&mut self, idx: usize) {
    let mut q = idx - 1;
    while q != idx {
      match self.body_node(q) {
        Node::Boundary { last_for_next, .. } => {
          q = *last_for_next;
        }
        Node::Normal {
          item_node,
          node_type: NodeType::Body { top, color },
        } => {
          let top = *top as usize;

          if self.header(top).is_primary() || color.is_some() {
            let prev_idx = item_node.prev;
            let next_idx = item_node.next;
            self.node_mut(prev_idx).set_next(q);
            self.node_mut(next_idx).set_prev(q);
          }
          *self.body_header_mut(top).len_mut() += 1;
          q -= 1;
        }
        Node::Normal {
          node_type: NodeType::Header { .. },
          ..
        } => unreachable!("Unexpected header encountered in unhide() at index {q}"),
      }
    }
    // println!("Unhiding {idx}");
  }

  /// Remove all subsets which contain the header item `idx`, and hide the item
  /// from the items list.
  fn cover(&mut self, idx: usize) {
    // println!("Covering {:?}", self.header(idx).item.as_ref().unwrap());
    debug_assert!((1..=self.num_primary_items).contains(&idx));
    let mut p = self.body_header(idx).next();
    while p != idx {
      self.hide(p);
      p = self.body_node(p).next();
    }

    // Hide this item in the items list.
    let header = self.header(idx);
    let prev_idx = header.node.prev;
    let next_idx = header.node.next;
    self.header_mut(prev_idx as usize).node.next = next_idx;
    self.header_mut(next_idx as usize).node.prev = prev_idx;
  }

  /// Reverts `cover(idx)`, assuming the state of Dlx was exactly as it was
  /// when `cover(idx)` was called.
  fn uncover(&mut self, idx: usize) {
    debug_assert!((1..=self.num_primary_items).contains(&idx));
    // Put this item back in the items list.
    let header = self.header(idx);
    let prev_idx = header.node.prev;
    let next_idx = header.node.next;
    self.header_mut(prev_idx as usize).node.next = idx as u32;
    self.header_mut(next_idx as usize).node.prev = idx as u32;

    let mut p = self.body_header(idx).prev();
    while p != idx {
      self.unhide(p);
      p = self.body_node(p).prev();
    }
    // println!("Uncovering {:?}", self.header(idx).item.as_ref().unwrap());
  }

  /// Covers all subsets with secondary constraints which don't have the same
  /// color as the constraint at index `idx`.
  fn purify(&mut self, idx: usize) {
    let (color, top) = match self.body_node(idx) {
      Node::Normal {
        node_type: NodeType::Body {
          color: Some(color),
          top,
        },
        ..
      } => (*color, *top as usize),
      _ => unreachable!("Unexpected uncolored node for secondary constraint at index {idx}."),
    };
    // println!("Purifying {idx} (top {top}, color {color})");

    let mut p = self.body_header(top).next();
    while p != top {
      let p_color = self.body_node_mut(p).color_mut();
      // println!("Looking at {p} ({p_color:?})");
      if *p_color == Some(color) {
        *p_color = None;
      } else {
        self.hide(p);
      }
      p = self.body_node(p).next();
    }
  }

  /// Reverts `purify(idx)`, assuming the state of Dlx was exactly as it was
  /// when `purify(idx)` was called.
  fn unpurify(&mut self, idx: usize) {
    let (color, top) = match self.body_node(idx) {
      Node::Normal {
        node_type: NodeType::Body {
          color: Some(color),
          top,
        },
        ..
      } => (*color, *top as usize),
      _ => unreachable!("Unexpected uncolored node for secondary constraint at index {idx}."),
    };

    let mut p = self.body_header(top).prev();
    while p != top {
      let p_color = self.body_node_mut(p).color_mut();
      if p_color.is_none() {
        *p_color = Some(color);
      } else {
        self.unhide(p);
      }
      p = self.body_node(p).prev();
    }
    // println!("Unpurifying {idx}");
  }

  fn commit(&mut self, idx: usize, top: usize) {
    // println!("Committing {idx} (top: {top})");
    if self.header(top).is_primary() {
      self.cover(top);
    } else if self.body_node(idx).color().is_some() {
      self.purify(idx);
    }
  }

  fn uncommit(&mut self, idx: usize, top: usize) {
    if self.header(top).is_primary() {
      self.uncover(top);
    } else if self.body_node(idx).color().is_some() {
      self.unpurify(idx);
    }
    // println!("Uncommitting {idx} (top: {top})");
  }

  /// Covers all other items take by the subset containing the node at `idx`.
  fn cover_remaining_choices(&mut self, idx: usize) {
    // println!("Covering remaining for {idx}");
    let mut p = idx + 1;
    while p != idx {
      match self.body_node(p) {
        Node::Boundary { first_for_prev, .. } => {
          p = *first_for_prev;
        }
        Node::Normal {
          node_type: NodeType::Body { top, .. },
          ..
        } => {
          self.commit(p, *top as usize);
          p += 1;
        }
        Node::Normal {
          node_type: NodeType::Header { .. },
          ..
        } => {
          unreachable!("Unexpected header encountered in cover_remaining_choices() at index {p}")
        }
      }
    }
  }

  /// Covers all other items take by the subset containing the node at `idx`.
  fn uncover_remaining_choices(&mut self, idx: usize) {
    let mut p = idx - 1;
    while p != idx {
      match self.body_node(p) {
        Node::Boundary { last_for_next, .. } => {
          p = *last_for_next;
        }
        Node::Normal {
          node_type: NodeType::Body { top, .. },
          ..
        } => {
          self.uncommit(p, *top as usize);
          p -= 1;
        }
        Node::Normal {
          node_type: NodeType::Header { .. },
          ..
        } => {
          unreachable!("Unexpected header encountered in uncover_remaining_choices() at index {p}")
        }
      }
    }
    // println!("Uncovering remaining for {idx}");
  }

  /// Chooses the index of the next item to try covering, using the LRV
  /// heuristic (least remaining values). Returns None if there are no items
  /// left, meaning a solution has been found.
  fn choose_item(&self) -> Option<u32> {
    let mut opt = self.header(0).node.next;
    let mut best_opt = (None, 0);
    while opt != 0 {
      let len = self.body_header(opt as usize).len();
      best_opt = match best_opt {
        (Some(_), min_len) => {
          if min_len > len {
            (Some(opt), len)
          } else {
            best_opt
          }
        }
        (None, _) => (Some(opt), len),
      };

      opt = self.header(opt as usize).node.next;
    }

    best_opt.0
  }

  fn set_name_for_node(&self, idx: usize) -> N {
    ((idx + 1)..)
      .find_map(|q| match self.body_node(q) {
        Node::Boundary { name, .. } => Some(name.clone().unwrap()),
        Node::Normal { .. } => None,
      })
      .unwrap()
  }

  fn items_for_node(&self, idx: usize) -> impl Iterator<Item = Constraint<I>> + '_ {
    self
      .iterate_items(idx)
      .map(move |item_idx| match self.body_node(item_idx).color() {
        Some(color) => ColorItem::new(self.item_name(item_idx), color).into(),
        None => self.item_name(item_idx).into(),
      })
  }

  fn find_all_solutions_idx(&mut self) -> Vec<Vec<usize>>
  where
    I: Debug,
    N: Debug,
  {
    let mut solutions = Vec::new();
    let mut solution = Vec::new();

    'cover_new_item: loop {
      match self.choose_item() {
        Some(item) => {
          let item = item as usize;
          solution.push(item);
          self.cover(item);
        }
        None => {
          // Undo all changes we've made to the data structure.
          // solution.iter().rev().for_each(|&p| {
          //   self.uncover_remaining_choices(p);
          //   let top = self.body_node(p).top() as usize;
          //   self.uncover(top);
          // });
          solutions.push(solution.clone());
        }
      }
      // println!("d{} for {}", solution.len(), solution.last().unwrap());

      while let Some(p) = solution.pop() {
        if let Node::Normal {
          node_type: NodeType::Body { .. },
          ..
        } = self.node(p)
        {
          self.uncover_remaining_choices(p);
        }

        // Try exploring the next choice.
        let p = self.node(p).next();

        match self.node(p) {
          Node::Normal {
            node_type: NodeType::Header { .. },
            ..
          } => {
            // We have exhausted all options under this item, so continue to the
            // previous item.
            self.uncover(p);
          }
          Node::Normal {
            node_type: NodeType::Body { .. },
            ..
          } => {
            // We can try exploring this subset.
            solution.push(p);
            self.cover_remaining_choices(p);
            continue 'cover_new_item;
          }
          Node::Boundary { .. } => unreachable!("Unexpected boundary node found in queue: {p}"),
        }
      }

      break;
    }

    solutions
  }

  pub fn find_solution_names(&mut self) -> Option<impl Iterator<Item = N> + '_>
  where
    I: Debug,
    N: Debug,
  {
    let mut solutions = self.find_all_solutions_idx();
    debug_assert_eq!(solutions.len(), 1);
    solutions
      .pop()
      .map(|solution| solution.into_iter().map(|p| self.set_name_for_node(p)))
  }

  pub fn find_all_solution_colors(&mut self) -> impl Iterator<Item = HashMap<I, u32>> + '_
  where
    I: Debug,
    N: Debug,
  {
    self.find_all_solutions_idx().into_iter().map(|solution| {
      solution
        .iter()
        .fold(HashMap::new(), |secondary_assignments, &p| {
          self
            .items_for_node(p)
            .fold(secondary_assignments, |mut secondary_assignments, c| {
              if let Constraint::Secondary(ColorItem { item, color }) = c {
                if let Some(prev_color) = secondary_assignments.insert(item, color) {
                  debug_assert_eq!(color, prev_color);
                }
              }
              secondary_assignments
            })
        })
    })
  }

  pub fn find_solution_colors(&mut self) -> Option<HashMap<I, u32>>
  where
    I: Debug,
    N: Debug,
  {
    let mut solutions = self.find_all_solutions_idx();
    debug_assert_eq!(solutions.len(), 1);
    solutions.pop().map(|solution| {
      solution
        .iter()
        .fold(HashMap::new(), |secondary_assignments, &p| {
          self
            .items_for_node(p)
            .fold(secondary_assignments, |mut secondary_assignments, c| {
              if let Constraint::Secondary(ColorItem { item, color }) = c {
                if let Some(prev_color) = secondary_assignments.insert(item, color) {
                  debug_assert_eq!(color, prev_color);
                }
              }
              secondary_assignments
            })
        })
    })
  }
}

impl<I, N> Debug for Dlx<I, N>
where
  I: Debug,
  N: Debug,
{
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    for (idx, header) in self.headers.iter().enumerate() {
      writeln!(f, "{idx:<3} H: {header:?}")?;
    }
    for (idx, node) in self.body.iter().enumerate() {
      writeln!(f, "{idx:<3} N: {:?}", node)?;
    }
    Ok(())
  }
}

#[cfg(test)]
mod test {
  use itertools::Itertools;

  use crate::dlx::{ColorItem, Constraint};

  use super::{Dlx, HeaderType};

  #[test]
  fn test_empty() {
    let mut dlx: Dlx<u32, u32> = Dlx::new::<_, _, Vec<_>, u32>(vec![], vec![]);

    assert!(dlx
      .find_solution_names()
      .is_some_and(|solution| solution.eq(vec![].into_iter())));
  }

  #[test]
  fn test_simple() {
    let mut dlx = Dlx::new(vec![(1, HeaderType::Primary)], vec![(0, vec![1])]);

    assert!(dlx
      .find_solution_names()
      .is_some_and(|solution| solution.eq(vec![0].into_iter())));
  }

  #[test]
  fn test_choose_two() {
    let mut dlx = Dlx::new(
      vec![
        ('p', HeaderType::Primary),
        ('q', HeaderType::Primary),
        ('r', HeaderType::Primary),
      ],
      vec![
        (0, vec!['p', 'q']),
        (1, vec!['p', 'r']),
        (2, vec!['p']),
        (3, vec!['q']),
      ],
    );

    assert!(dlx
      .find_solution_names()
      .is_some_and(|solution| { solution.sorted().eq(vec![1, 3].into_iter()) }));
  }

  #[test]
  fn test_simple_colors() {
    let mut dlx = Dlx::new(
      vec![
        ('p', HeaderType::Primary),
        ('q', HeaderType::Primary),
        ('a', HeaderType::Secondary),
      ],
      vec![
        (
          0,
          vec![Constraint::Primary('p'), ColorItem::new('a', 1).into()],
        ),
        (1, vec!['p'.into(), ColorItem::new('a', 2).into()]),
        (2, vec!['q'.into(), ColorItem::new('a', 3).into()]),
        (3, vec!['q'.into(), ColorItem::new('a', 1).into()]),
      ],
    );

    assert!(dlx
      .find_solution_names()
      .is_some_and(|solution| { solution.sorted().eq(vec![0, 3].into_iter()) }));
  }
}
