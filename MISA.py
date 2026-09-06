import csv
import ctypes
import heapq
import multiprocessing as mp
import os
import random
import sys
import time
import traceback
import tracemalloc
from array import array
from dataclasses import dataclass

import igraph as ig


RANDOM_SEED = 42

GENERAL_NODE_COUNT = 100
GENERAL_EDGE_PROBABILITY = 0.04
# Keep the graph-generation seed aligned with RANDOM_SEED.  Algorithms receive
# the same seed separately for their deterministic/randomized tie-breaks.
GENERAL_SEEDS = (RANDOM_SEED,)
RANDOM_GREEDY_STARTS = 10
GRASP_ITERATIONS = 20
GRASP_RCL_SIZE = 8
ILS_ITERATIONS = 40
ILS_PERTURBATION_FRACTION = 0.25
EXACT_ALGORITHM_NODE_LIMIT = 1000
CPLEX_EXACT_NODE_LIMIT = EXACT_ALGORITHM_NODE_LIMIT
COLOR_BNB_NODE_LIMIT = EXACT_ALGORITHM_NODE_LIMIT
KERNEL_BNB_NODE_LIMIT = EXACT_ALGORITHM_NODE_LIMIT
BRUTE_FORCE_NODE_LIMIT = EXACT_ALGORITHM_NODE_LIMIT
ALGORITHM_TIMEOUT_SECONDS = 600.0
# A second, tracemalloc-instrumented run profiles each algorithm's own heap
# allocation. It roughly doubles (and, with the profiler's per-allocation
# overhead, can more than triple) the runtime, so it is only performed when the
# clean timed run stayed below this budget. The profiling pass is a second run,
# so algorithms that consume a substantial part of ALGORITHM_TIMEOUT_SECONDS can
# still time out while their memory is being measured.
MEMORY_PROFILE_MAX_SECONDS = 3600.0
INCREMENTAL_RESULTS_CSV = "misa_incremental_results.csv"
# Every generated random graph is stored here as its own text file so that any
# experiment can be repeated later on exactly the same instances.
GENERAL_GRAPH_DIR = "general_graph_instances"
CPLEX_QUBO_PENALTY = 2.0

ACADEMIC_REFERENCES = [
    (
        "R1",
        "M. M. Halldorsson and J. Radhakrishnan, 'Greed is Good: "
        "Approximating Independent Sets in Sparse and Bounded-Degree Graphs,' "
        "Algorithmica, vol. 18, no. 1, pp. 145-163, 1997, "
        "doi:10.1007/BF02523693.",
    ),
    (
        "R2",
        "E. Hemaspaandra, J. Rothe, and H. Spakowski, 'Recognizing When "
        "Heuristics Can Approximate Minimum Vertex Covers Is Complete for "
        "Parallel Access to NP,' RAIRO-Theoretical Informatics and "
        "Applications, vol. 40, no. 1, pp. 75-91, 2006, "
        "doi:10.1051/ita:2005041.",
    ),
    (
        "R3",
        "G. E. Blelloch, J. T. Fineman, and J. Shun, 'Greedy Sequential "
        "Maximal Independent Set and Matching are Parallel on Average,' "
        "Proc. 24th ACM SPAA, pp. 308-317, 2012, "
        "doi:10.1145/2312005.2312058.",
    ),
    (
        "R4",
        "S. Balaji, V. Swaminathan, and K. Kannan, 'A Simple Algorithm to "
        "Optimize Maximum Independent Set,' Advanced Modeling and "
        "Optimization, vol. 12, no. 1, pp. 107-118, 2010, "
        "https://camo.ici.ro/journal/vol12/v12a9.pdf.",
    ),
    (
        "R5",
        "M. Luby, 'A Simple Parallel Algorithm for the Maximal Independent "
        "Set Problem,' SIAM Journal on Computing, vol. 15, no. 4, "
        "pp. 1036-1053, 1986, doi:10.1137/0215074.",
    ),
    (
        "R6",
        "D. W. Matula and L. L. Beck, 'Smallest-Last Ordering and "
        "Clustering and Graph Coloring Algorithms,' Journal of the ACM, "
        "vol. 30, no. 3, pp. 417-427, 1983, doi:10.1145/2402.322385.",
    ),
    (
        "R7",
        "L. Chang, W. Li, and W. Zhang, 'Computing A Near-Maximum "
        "Independent Set in Linear Time by Reducing-Peeling,' Proc. ACM "
        "SIGMOD, pp. 1181-1196, 2017, doi:10.1145/3035918.3035939.",
    ),
    (
        "R8",
        "D. V. Andrade, M. G. C. Resende, and R. F. Werneck, 'Fast Local "
        "Search for the Maximum Independent Set Problem,' Journal of "
        "Heuristics, vol. 18, no. 4, pp. 525-547, 2012, "
        "doi:10.1007/s10732-012-9196-4.",
    ),
    (
        "R9",
        "T. A. Feo, M. G. C. Resende, and S. H. Smith, 'A Greedy "
        "Randomized Adaptive Search Procedure for Maximum Independent Set,' "
        "Operations Research, vol. 42, no. 5, pp. 860-878, 1994, "
        "doi:10.1287/opre.42.5.860.",
    ),
    (
        "R10",
        "B. C. S. Nogueira, R. G. S. Pinheiro, and A. Subramanian, 'A "
        "Hybrid Iterated Local Search Heuristic for the Maximum Weight "
        "Independent Set Problem,' Optimization Letters, vol. 12, no. 3, "
        "pp. 567-583, 2018, doi:10.1007/s11590-017-1128-7.",
    ),
    (
        "R11",
        "E. Boros and P. L. Hammer, 'Pseudo-Boolean Optimization,' "
        "Discrete Applied Mathematics, vol. 123, no. 1-3, pp. 155-225, "
        "2002, doi:10.1016/S0166-218X(01)00341-9.",
    ),
    (
        "R12",
        "A. Lucas, 'Ising Formulations of Many NP Problems,' Frontiers in "
        "Physics, vol. 2, art. 5, 2014, doi:10.3389/fphy.2014.00005.",
    ),
    (
        "R13",
        "A. H. Land and A. G. Doig, 'An Automatic Method of Solving "
        "Discrete Programming Problems,' Econometrica, vol. 28, no. 3, "
        "pp. 497-520, 1960, doi:10.2307/1910129.",
    ),
    (
        "R14",
        "E. Tomita and T. Kameda, 'An Efficient Branch-and-Bound Algorithm "
        "for Finding a Maximum Clique with Computational Experiments,' "
        "Journal of Global Optimization, vol. 37, no. 1, pp. 95-111, 2007, "
        "doi:10.1007/s10898-006-9039-7.",
    ),
    (
        "R15",
        "S. Lamm, C. Schulz, D. Strash, R. Williger, and H. Zhang, "
        "'Exactly Solving the Maximum Weight Independent Set Problem on "
        "Large Real-World Graphs,' Proc. ALENEX, pp. 144-158, 2019, "
        "doi:10.1137/1.9781611975499.12.",
    ),
    (
        "R16",
        "R. E. Tarjan and A. E. Trojanowski, 'Finding a Maximum Independent "
        "Set,' SIAM Journal on Computing, vol. 6, no. 3, pp. 537-546, 1977, "
        "doi:10.1137/0206038.",
    ),
]


class AlgorithmSkipped(Exception):
    """Raised when an optional solver cannot produce a proven exact result."""


@dataclass(frozen=True)
class GeneralGraphData:
    graph: ig.Graph
    edges: list[tuple[int, int]]


def create_random_general_graph(
    node_count: int,
    edge_probability: float,
    seed: int,
) -> GeneralGraphData:
    rng = random.Random(seed)
    edges = []

    for source in range(node_count):
        for target in range(source + 1, node_count):
            if rng.random() < edge_probability:
                edges.append((source, target))

    graph = ig.Graph(n=node_count, edges=edges, directed=False)
    graph.vs["original_id"] = list(range(node_count))

    return GeneralGraphData(graph, edges)


def save_general_graph_txt(
    data: GeneralGraphData,
    node_count: int,
    edge_probability: float,
    seed: int,
    directory: str = GENERAL_GRAPH_DIR,
) -> str:
    """Store one generated random graph as a text file for experiment repeatability."""
    os.makedirs(directory, exist_ok=True)
    probability_tag = f"{edge_probability:g}".replace(".", "p")
    path = os.path.join(
        directory,
        f"graph_n{node_count}_p{probability_tag}_seed{seed}.txt",
    )

    with open(path, "w", encoding="utf-8") as graph_file:
        graph_file.write("# Erdos-Renyi general graph G(n, p)\n")
        graph_file.write(
            f"# nodes={node_count} edge_probability={edge_probability} "
            f"seed={seed} edges={len(data.edges)}\n"
        )
        graph_file.write(
            "# first data line: '<node count> <edge count>', "
            "then one '<source> <target>' pair per line\n"
        )
        graph_file.write(f"{node_count} {len(data.edges)}\n")
        for source, target in data.edges:
            graph_file.write(f"{source} {target}\n")

    return path


def malatya_centrality(graph: ig.Graph) -> list[float]:
    values = []

    for vertex in range(graph.vcount()):
        vertex_degree = graph.degree(vertex)
        neighbors = graph.neighbors(vertex)

        if not neighbors:
            values.append(0.0)
            continue

        neighbor_degrees = graph.degree(neighbors)
        centrality_value = sum(vertex_degree / degree for degree in neighbor_degrees)
        values.append(centrality_value)

    return values


def find_minimum(graph: ig.Graph) -> int:
    centralities = malatya_centrality(graph)
    return min(range(len(centralities)), key=lambda index: centralities[index])


def misa_greedy_full_scan(graph: ig.Graph) -> list[int]:
    """Reference implementation that recomputes all active scores each round."""
    adjacency = graph.get_adjlist()
    original_ids = (
        graph.vs["original_id"]
        if "original_id" in graph.vs.attributes()
        else list(range(graph.vcount()))
    )
    node_count = graph.vcount()
    active = [True] * node_count
    active_count = node_count
    degrees = [len(neighbors) for neighbors in adjacency]
    independent_set = []

    def remove_node(node: int) -> None:
        nonlocal active_count
        if not active[node]:
            return

        active[node] = False
        active_count -= 1
        for neighbor in adjacency[node]:
            if active[neighbor]:
                degrees[neighbor] -= 1

    while active_count > 0:
        isolated_nodes = [
            node
            for node in range(node_count)
            if active[node] and degrees[node] == 0
        ]
        if isolated_nodes:
            independent_set.extend(original_ids[node] for node in isolated_nodes)
            for node in isolated_nodes:
                remove_node(node)
            continue

        def centrality(node: int) -> float:
            node_degree = degrees[node]
            return sum(
                node_degree / degrees[neighbor]
                for neighbor in adjacency[node]
                if active[neighbor]
            )

        min_node = min(
            (node for node in range(node_count) if active[node]),
            key=lambda node: (centrality(node), node),
        )
        independent_set.append(original_ids[min_node])

        nodes_to_delete = [min_node] + [
            neighbor for neighbor in adjacency[min_node] if active[neighbor]
        ]
        for node in nodes_to_delete:
            remove_node(node)

    return sorted(independent_set)


def build_csr_adjacency(
    node_count: int,
    edges: list[tuple[int, int]],
) -> tuple[array, array, array]:
    # 16-bit ids when they fit (node_count <= 65535) halve the dominant
    # neighbours buffer at no runtime cost; offsets index 2E entries so stay 32-bit.
    id_code = "H" if node_count <= 65535 else "I"
    degrees = array(id_code, [0]) * node_count
    for source, target in edges:
        degrees[source] += 1
        degrees[target] += 1

    offsets = array("I", [0]) * (node_count + 1)
    running_total = 0
    for node, degree in enumerate(degrees):
        offsets[node] = running_total
        running_total += degree
    offsets[node_count] = running_total

    neighbors = array(id_code, [0]) * running_total
    # offsets[:-1] already yields an independent, mutable copy; wrapping it in
    # another array() call would allocate a second copy for no reason.
    cursor = offsets[:-1]
    for source, target in edges:
        source_position = cursor[source]
        neighbors[source_position] = target
        cursor[source] += 1

        target_position = cursor[target]
        neighbors[target_position] = source
        cursor[target] += 1

    return offsets, neighbors, degrees


def build_csr_adjacency_from_graph(graph: ig.Graph) -> tuple[array, array, array]:
    node_count = graph.vcount()
    id_code = "H" if node_count <= 65535 else "I"
    degrees = array(id_code, [0]) * node_count

    for edge in graph.es:
        degrees[edge.source] += 1
        degrees[edge.target] += 1

    offsets = array("I", [0]) * (node_count + 1)
    running_total = 0
    for node, degree in enumerate(degrees):
        offsets[node] = running_total
        running_total += degree
    offsets[node_count] = running_total

    neighbors = array(id_code, [0]) * running_total
    # offsets[:-1] already yields an independent, mutable copy; wrapping it in
    # another array() call would allocate a second copy for no reason.
    cursor = offsets[:-1]
    for edge in graph.es:
        source = edge.source
        target = edge.target

        source_position = cursor[source]
        neighbors[source_position] = target
        cursor[source] += 1

        target_position = cursor[target]
        neighbors[target_position] = source
        cursor[target] += 1

    return offsets, neighbors, degrees


def misa_greedy_from_csr(
    node_count: int,
    offsets: array,
    neighbors: array,
    degrees: array,
    original_ids: list[int] | None = None,
) -> list[int]:
    """Memory-compact heap-based MISA implementation over a CSR graph.

    This is a performance refactor of the reference greedy. The selection rule
    is unchanged: repeatedly take the active vertex with minimum Malatya
    centrality (ties broken by vertex index) and delete its closed
    neighbourhood, recomputing centralities of the affected vertices. Only the
    bookkeeping is optimised, so the returned independent set is byte-identical
    to the reference implementation on every graph:

    * The neighbour generator is inlined and the CSR arrays are bound to locals.
    * The three transient per-round flag arrays (delete / first-shell /
      affected) are packed into a single byte array.
    * Each vertex's current centrality is memoised in a compact ``array('d')``
      (``best``). Because a vertex's centrality only changes when it is in the
      affected set (where we recompute and refresh ``best``), a stale heap entry
      is detected in O(1) by comparing its stored score against ``best`` instead
      of recomputing the centrality, and the heap can be rebuilt straight from
      ``best`` without recomputation. This makes frequent, cheap compactions
      affordable, which in turn keeps the heap small (lower peak memory).
    """
    DELETE_FLAG = 1
    SHELL_FLAG = 2
    AFFECTED_FLAG = 4

    active = bytearray([1]) * node_count
    active_count = node_count
    # One byte per vertex holds all transient per-round flags via bit masks,
    # replacing three separate byte arrays (delete / first-shell / affected).
    marks = bytearray(node_count)
    # Memoised current centrality per vertex (8 bytes each, no boxed floats).
    best = array("d", [0.0]) * node_count
    heap = []
    independent_set = []

    # Bind hot globals and CSR arrays to locals to avoid attribute/global
    # lookups inside the innermost loops.
    push = heapq.heappush
    pop = heapq.heappop
    heapify = heapq.heapify
    heap_append = heap.append
    heap_clear = heap.clear
    offs = offsets
    nbrs = neighbors
    degs = degrees
    act = active

    def centrality(node, offs=offs, nbrs=nbrs, degs=degs, act=act):
        node_degree = degs[node]
        if node_degree == 0:
            return 0.0

        # Must use the builtin sum(): on CPython 3.12+ sum() applies Neumaier
        # compensated summation for floats, whereas a manual "+=" loop uses naive
        # addition. The two can differ by 1 ULP, which is enough to flip an exact
        # centrality tie and make the greedy pick a different vertex. Keeping
        # sum() (over the same neighbours in the same order) makes this identical
        # to the reference implementation.
        return sum(
            node_degree / degs[neighbor]
            for position in range(offs[node], offs[node + 1])
            if act[(neighbor := nbrs[position])]
        )

    def rebuild_heap():
        # Compact straight from the memoised centralities; no recomputation and
        # no temporary list (append in place, then heapify).
        heap_clear()
        for node in range(node_count):
            if act[node]:
                heap_append((best[node], node))
        heapify(heap)

    for node in range(node_count):
        if act[node]:
            best[node] = centrality(node)
    rebuild_heap()

    while active_count > 0:
        while heap:
            score, min_node = pop(heap)
            if not act[min_node]:
                continue

            # O(1) staleness test: best[min_node] is this vertex's current
            # centrality, so a matching score means the entry is fresh. Stale
            # entries are simply dropped -- the fresh (best, node) entry is
            # always already in the heap.
            if score == best[min_node]:
                break
        else:
            raise RuntimeError("Priority queue was exhausted before the graph was empty.")

        if original_ids is None:
            independent_set.append(min_node)
        else:
            independent_set.append(original_ids[min_node])

        nodes_to_delete = [min_node]
        marks[min_node] = DELETE_FLAG
        for position in range(offs[min_node], offs[min_node + 1]):
            neighbor = nbrs[position]
            if act[neighbor] and not (marks[neighbor] & DELETE_FLAG):
                marks[neighbor] |= DELETE_FLAG
                nodes_to_delete.append(neighbor)

        first_shell = []

        for deleted_node in nodes_to_delete:
            for position in range(offs[deleted_node], offs[deleted_node + 1]):
                neighbor = nbrs[position]
                flags = marks[neighbor]
                if (
                    act[neighbor]
                    and not (flags & DELETE_FLAG)
                    and not (flags & SHELL_FLAG)
                ):
                    marks[neighbor] = flags | SHELL_FLAG
                    first_shell.append(neighbor)

        for deleted_node in nodes_to_delete:
            if not act[deleted_node]:
                continue

            act[deleted_node] = 0
            active_count -= 1

            for position in range(offs[deleted_node], offs[deleted_node + 1]):
                neighbor = nbrs[position]
                if act[neighbor]:
                    degs[neighbor] -= 1

        affected_nodes = []
        for node in first_shell:
            if act[node] and not (marks[node] & AFFECTED_FLAG):
                marks[node] |= AFFECTED_FLAG
                affected_nodes.append(node)

            for position in range(offs[node], offs[node + 1]):
                neighbor = nbrs[position]
                if act[neighbor] and not (marks[neighbor] & AFFECTED_FLAG):
                    marks[neighbor] |= AFFECTED_FLAG
                    affected_nodes.append(neighbor)

        for node in affected_nodes:
            current_score = centrality(node)
            best[node] = current_score
            push(heap, (current_score, node))

        for node in nodes_to_delete:
            marks[node] = 0
        for node in first_shell:
            marks[node] = 0
        for node in affected_nodes:
            marks[node] = 0

        # Compact the heap once stale entries pile up. Rebuilds are cheap (they
        # reuse the memoised centralities), so the slack is kept tight to hold
        # the heap -- the dominant allocation -- small. The additive +128 term
        # dominates on small graphs (maximal memory saving); the multiplicative
        # (5/4) term takes over on large graphs so rebuilds stay amortized O(1)
        # and never become a runtime bottleneck. Rebuild timing never affects
        # the result, only the resident slack.
        if len(heap) > max(active_count + 128, (5 * active_count) // 4):
            rebuild_heap()

    return sorted(independent_set)


def misa_greedy_from_edges(
    node_count: int,
    edges: list[tuple[int, int]],
    original_ids: list[int] | None = None,
) -> list[int]:
    offsets, neighbors, degrees = build_csr_adjacency(node_count, edges)
    return misa_greedy_from_csr(
        node_count,
        offsets,
        neighbors,
        degrees,
        original_ids,
    )


def misa_greedy(graph: ig.Graph) -> list[int]:
    """Compatibility wrapper for graph-based calls."""
    original_ids = (
        graph.vs["original_id"]
        if "original_id" in graph.vs.attributes()
        else None
    )
    offsets, neighbors, degrees = build_csr_adjacency_from_graph(graph)
    return misa_greedy_from_csr(
        graph.vcount(),
        offsets,
        neighbors,
        degrees,
        original_ids,
    )


def is_independent_set(
    edges: list[tuple[int, int]],
    independent_set: list[int],
) -> bool:
    selected = set(independent_set)
    return all(
        source not in selected or target not in selected
        for source, target in edges
    )


def build_adjacency_bitsets(
    node_count: int,
    edges: list[tuple[int, int]],
) -> list[int]:
    adjacency = [0] * node_count
    for source, target in edges:
        adjacency[source] |= 1 << target
        adjacency[target] |= 1 << source
    return adjacency


def bitset_to_sorted_list(mask: int) -> list[int]:
    vertices = []
    while mask:
        bit = mask & -mask
        vertices.append(bit.bit_length() - 1)
        mask &= ~bit
    return vertices


def brute_force_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
) -> list[int]:
    if node_count > BRUTE_FORCE_NODE_LIMIT:
        raise ValueError(
            "Brute force is intentionally limited to "
            f"n <= {BRUTE_FORCE_NODE_LIMIT}."
        )

    adjacency = build_adjacency_bitsets(node_count, edges)
    best_mask = 0
    best_size = 0

    def search(vertex: int, selected: int, selected_size: int) -> None:
        nonlocal best_mask, best_size

        if selected_size + (node_count - vertex) <= best_size:
            return

        if vertex == node_count:
            if selected_size > best_size:
                best_size = selected_size
                best_mask = selected
            return

        if not (selected & adjacency[vertex]):
            search(vertex + 1, selected | (1 << vertex), selected_size + 1)
        search(vertex + 1, selected, selected_size)

    search(0, 0, 0)
    return bitset_to_sorted_list(best_mask)


def branch_and_bound_mask(
    node_count: int,
    adjacency: list[int],
    candidate_mask: int,
) -> int:
    all_vertices = (1 << node_count) - 1
    complement_adjacency = [
        all_vertices & ~(adjacency[vertex] | (1 << vertex))
        for vertex in range(node_count)
    ]
    best_mask = 0
    best_size = 0

    def color_sort(candidates: int) -> tuple[list[int], list[int]]:
        order = []
        bounds = []
        color = 0
        uncolored = candidates

        while uncolored:
            color += 1
            available = uncolored

            while available:
                bit = available & -available
                vertex = bit.bit_length() - 1
                order.append(vertex)
                bounds.append(color)

                uncolored &= ~bit
                available &= ~bit
                available &= ~complement_adjacency[vertex]

        return order, bounds

    def expand(candidates: int, selected: int, selected_size: int) -> None:
        nonlocal best_mask, best_size

        if not candidates:
            if selected_size > best_size:
                best_size = selected_size
                best_mask = selected
            return

        order, bounds = color_sort(candidates)
        remaining = candidates

        for index in range(len(order) - 1, -1, -1):
            if selected_size + bounds[index] <= best_size:
                return

            vertex = order[index]
            bit = 1 << vertex
            if remaining & bit:
                expand(
                    remaining & complement_adjacency[vertex],
                    selected | bit,
                    selected_size + 1,
                )
                remaining &= ~bit

    expand(candidate_mask, 0, 0)
    return best_mask


def branch_and_bound_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
) -> list[int]:
    adjacency = build_adjacency_bitsets(node_count, edges)
    all_vertices = (1 << node_count) - 1
    best_mask = branch_and_bound_mask(node_count, adjacency, all_vertices)
    return bitset_to_sorted_list(best_mask)


def active_neighborhood_is_clique(adjacency: list[int], neighborhood: int) -> bool:
    current = neighborhood
    while current:
        bit = current & -current
        vertex = bit.bit_length() - 1
        current &= ~bit

        if current & ~adjacency[vertex]:
            return False

    return True


def exact_reduction_kernel_mask(
    node_count: int,
    adjacency: list[int],
    active: int,
) -> tuple[int, int]:
    forced = 0

    while active:
        chosen_bit = 0
        chosen_neighbors = 0
        current = active

        while current:
            bit = current & -current
            vertex = bit.bit_length() - 1
            neighborhood = adjacency[vertex] & active
            degree = neighborhood.bit_count()

            if degree <= 1 or active_neighborhood_is_clique(adjacency, neighborhood):
                chosen_bit = bit
                chosen_neighbors = neighborhood
                break

            current &= ~bit

        if not chosen_bit:
            break

        forced |= chosen_bit
        active &= ~(chosen_bit | chosen_neighbors)

    return forced, active


def connected_component_masks(active: int, adjacency: list[int]) -> list[int]:
    components = []
    remaining = active

    while remaining:
        start = remaining & -remaining
        frontier = start
        component = 0

        while frontier:
            bit = frontier & -frontier
            frontier &= ~bit

            if component & bit:
                continue

            vertex = bit.bit_length() - 1
            component |= bit
            frontier |= adjacency[vertex] & active & ~component

        components.append(component)
        remaining &= ~component

    return components


def kernelized_branch_and_bound_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
) -> list[int]:
    adjacency = build_adjacency_bitsets(node_count, edges)
    forced, active = exact_reduction_kernel_mask(
        node_count,
        adjacency,
        (1 << node_count) - 1,
    )
    solution = forced

    for component in connected_component_masks(active, adjacency):
        solution |= branch_and_bound_mask(node_count, adjacency, component)

    return bitset_to_sorted_list(solution)


def cplex_qubo_exact_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
    seed: int = RANDOM_SEED,
) -> list[int]:
    """Solve maximum independent set exactly as a CPLEX binary QUBO.

    The maximized objective is

        sum(x_v) - 2 * sum(x_u * x_v for (u, v) in E).

    Since the edge-conflict penalty is greater than one, removing a selected
    vertex incident to any selected edge strictly improves the objective.
    Therefore every global optimum is independent and its objective value is
    its cardinality. The formulation follows Boros and Hammer (2002) and Lucas
    (2014); CPLEX must return its proven-optimal MIP status before the result is
    accepted as exact.
    """
    if node_count == 0:
        return []
    if CPLEX_QUBO_PENALTY <= 1.0:
        raise ValueError("The exact MIS QUBO requires a penalty greater than 1.")

    try:
        import cplex
    except ModuleNotFoundError as exc:
        raise AlgorithmSkipped(
            "CPLEX Python API is not installed; run 'py -3.11 -m pip install cplex'"
        ) from exc

    try:
        with cplex.Cplex() as model:
            model.set_log_stream(None)
            model.set_results_stream(None)
            model.set_warning_stream(None)
            model.set_error_stream(None)
            model.objective.set_sense(model.objective.sense.maximize)
            model.variables.add(
                obj=[1.0] * node_count,
                lb=[0.0] * node_count,
                ub=[1.0] * node_count,
                types="B" * node_count,
            )
            model.objective.set_quadratic_coefficients(
                [
                    (source, target, -CPLEX_QUBO_PENALTY)
                    for source, target in edges
                ]
            )
            model.parameters.threads.set(1)
            model.parameters.randomseed.set(seed)
            model.parameters.mip.tolerances.mipgap.set(0.0)
            model.parameters.mip.tolerances.absmipgap.set(0.0)
            model.solve()

            status = model.solution.get_status()
            if status != model.solution.status.MIP_optimal:
                raise AlgorithmSkipped(
                    "CPLEX did not prove global optimality: "
                    f"{model.solution.get_status_string(status)}"
                )

            values = model.solution.get_values()
            independent_set = [
                vertex for vertex, value in enumerate(values) if value > 0.5
            ]
            objective_value = model.solution.get_objective_value()
    except cplex.exceptions.CplexSolverError as exc:
        message = " ".join(str(exc).split())
        if "1016" in message or "size limits exceeded" in message.lower():
            raise AlgorithmSkipped(
                "CPLEX Community Edition size limit exceeded; a full academic "
                "or commercial CPLEX license is required"
            ) from exc
        raise

    if not is_independent_set(edges, independent_set):
        raise RuntimeError("CPLEX returned a non-independent QUBO solution.")

    if abs(objective_value - len(independent_set)) > 1e-6:
        raise RuntimeError(
            "CPLEX QUBO objective does not equal the independent-set size."
        )

    return independent_set


def min_degree_greedy_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
) -> list[int]:
    adjacency = build_adjacency_bitsets(node_count, edges)
    active = (1 << node_count) - 1
    selected = 0

    while active:
        best_vertex = None
        best_degree = None
        current = active

        while current:
            bit = current & -current
            vertex = bit.bit_length() - 1
            degree = (adjacency[vertex] & active).bit_count()
            if best_degree is None or degree < best_degree:
                best_vertex = vertex
                best_degree = degree
            current &= ~bit

        selected |= 1 << best_vertex
        active &= ~(1 << best_vertex)
        active &= ~adjacency[best_vertex]

    return bitset_to_sorted_list(selected)


def max_degree_removal_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
) -> list[int]:
    adjacency = build_adjacency_bitsets(node_count, edges)
    active = (1 << node_count) - 1

    while True:
        max_vertex = None
        max_degree = 0
        current = active

        while current:
            bit = current & -current
            vertex = bit.bit_length() - 1
            degree = (adjacency[vertex] & active).bit_count()
            if degree > max_degree:
                max_vertex = vertex
                max_degree = degree
            current &= ~bit

        if max_degree == 0:
            break

        active &= ~(1 << max_vertex)

    return bitset_to_sorted_list(active)


def random_greedy_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
    starts: int = RANDOM_GREEDY_STARTS,
    seed: int = RANDOM_SEED,
) -> list[int]:
    adjacency = build_adjacency_bitsets(node_count, edges)
    rng = random.Random(seed)
    best_mask = 0
    vertices = list(range(node_count))

    for _ in range(starts):
        order = vertices[:]
        rng.shuffle(order)
        selected = 0
        blocked = 0

        for vertex in order:
            bit = 1 << vertex
            if not (blocked & bit):
                selected |= bit
                blocked |= bit | adjacency[vertex]

        if selected.bit_count() > best_mask.bit_count():
            best_mask = selected

    return bitset_to_sorted_list(best_mask)


def vertex_support_greedy_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
) -> list[int]:
    """Vertex-support greedy heuristic for the maximum independent set.

    Reference:
        Balaji, S., Swaminathan, V., & Kannan, K. (2010). "A Simple Algorithm
        to Optimize Maximum Independent Set." Advanced Modeling and
        Optimization, 12(1), 107-118.

    The "support" of a vertex is the sum of the degrees of its neighbours. The
    algorithm greedily builds a vertex cover by repeatedly removing the vertex
    with the maximum support (ties broken by larger degree, then smaller index)
    until no edges remain; the vertices left outside the cover form the
    independent set (S = V - VC).
    """
    adjacency = build_adjacency_bitsets(node_count, edges)
    active = (1 << node_count) - 1

    while True:
        degree = [0] * node_count
        current = active
        has_edge = False
        while current:
            bit = current & -current
            vertex = bit.bit_length() - 1
            vertex_degree = (adjacency[vertex] & active).bit_count()
            degree[vertex] = vertex_degree
            if vertex_degree:
                has_edge = True
            current &= ~bit

        if not has_edge:
            break

        cover_vertex = None
        best_key = None
        current = active
        while current:
            bit = current & -current
            vertex = bit.bit_length() - 1

            support = 0
            active_neighbors = adjacency[vertex] & active
            while active_neighbors:
                neighbor_bit = active_neighbors & -active_neighbors
                support += degree[neighbor_bit.bit_length() - 1]
                active_neighbors &= ~neighbor_bit

            key = (support, degree[vertex])
            if best_key is None or key > best_key:
                best_key = key
                cover_vertex = vertex
            current &= ~bit

        active &= ~(1 << cover_vertex)

    return bitset_to_sorted_list(active)


def luby_randomized_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
    seed: int = RANDOM_SEED,
) -> list[int]:
    """Luby's randomized (Monte-Carlo) maximal independent set algorithm.

    Reference:
        Luby, M. (1986). "A Simple Parallel Algorithm for the Maximal
        Independent Set Problem." SIAM Journal on Computing, 15(4), 1036-1053.
        DOI: 10.1137/0215074. (The random-priority / random-permutation variant
        of Luby's parallel MIS method.)

    Each round every still-active vertex draws a random priority. A vertex that
    is a strict local minimum among its active neighbours (ties broken by index
    so that no two adjacent vertices ever win together) joins the independent
    set; the winners and all their neighbours are removed. The process repeats
    until no active vertices remain, yielding a maximal independent set.
    """
    adjacency = build_adjacency_bitsets(node_count, edges)
    rng = random.Random(seed)
    active = (1 << node_count) - 1
    selected = 0

    while active:
        priority = {}
        current = active
        while current:
            bit = current & -current
            vertex = bit.bit_length() - 1
            priority[vertex] = rng.random()
            current &= ~bit

        winners = 0
        current = active
        while current:
            bit = current & -current
            vertex = bit.bit_length() - 1
            vertex_priority = priority[vertex]

            is_local_minimum = True
            active_neighbors = adjacency[vertex] & active
            while active_neighbors:
                neighbor_bit = active_neighbors & -active_neighbors
                neighbor = neighbor_bit.bit_length() - 1
                neighbor_priority = priority[neighbor]
                if neighbor_priority < vertex_priority or (
                    neighbor_priority == vertex_priority and neighbor < vertex
                ):
                    is_local_minimum = False
                    break
                active_neighbors &= ~neighbor_bit

            if is_local_minimum:
                winners |= bit
            current &= ~bit

        selected |= winners

        removed = winners
        remaining_winners = winners
        while remaining_winners:
            bit = remaining_winners & -remaining_winners
            removed |= adjacency[bit.bit_length() - 1]
            remaining_winners &= ~bit
        active &= ~removed

    return bitset_to_sorted_list(selected)


def smallest_last_greedy_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
) -> list[int]:
    """Smallest-last (degeneracy) ordering greedy for maximum independent set.

    Reference (ordering):
        Matula, D. W., & Beck, L. L. (1983). "Smallest-last ordering and
        clustering and graph coloring algorithms." Journal of the ACM, 30(3),
        417-427. DOI: 10.1145/2402.322385.

    A smallest-last (degeneracy) ordering is built by repeatedly removing a
    current minimum-degree vertex and recording the removal order. The
    independent set is then filled greedily by scanning vertices in reverse
    removal order and adding a vertex whenever none of its neighbours are
    already selected -- the standard first-fit corollary of the ordering.
    """
    adjacency = build_adjacency_bitsets(node_count, edges)
    active = (1 << node_count) - 1
    removal_order = []

    while active:
        min_vertex = None
        min_degree = None
        current = active
        while current:
            bit = current & -current
            vertex = bit.bit_length() - 1
            degree = (adjacency[vertex] & active).bit_count()
            if min_degree is None or degree < min_degree:
                min_degree = degree
                min_vertex = vertex
                if degree == 0:
                    break
            current &= ~bit

        removal_order.append(min_vertex)
        active &= ~(1 << min_vertex)

    selected = 0
    for vertex in reversed(removal_order):
        if not (adjacency[vertex] & selected):
            selected |= 1 << vertex

    return bitset_to_sorted_list(selected)


def maximalize_independent_set(
    node_count: int,
    adjacency: list[int],
    selected: int,
    order: list[int] | None = None,
) -> int:
    blocked = selected
    current = selected

    while current:
        bit = current & -current
        vertex = bit.bit_length() - 1
        blocked |= adjacency[vertex]
        current &= ~bit

    if order is None:
        iterable = range(node_count)
    else:
        iterable = order

    for vertex in iterable:
        bit = 1 << vertex
        if not (blocked & bit):
            selected |= bit
            blocked |= bit | adjacency[vertex]

    return selected


def improve_independent_set_1_2(
    node_count: int,
    adjacency: list[int],
    selected: int,
) -> int:
    selected = maximalize_independent_set(node_count, adjacency, selected)

    improved = True
    while improved:
        improved = False

        for vertex in range(node_count):
            bit = 1 << vertex
            if not (selected & bit) and not (adjacency[vertex] & selected):
                selected |= bit
                improved = True

        if improved:
            continue

        selected_vertices = bitset_to_sorted_list(selected)
        for removed_vertex in selected_vertices:
            reduced = selected & ~(1 << removed_vertex)
            candidates = []

            for candidate in range(node_count):
                bit = 1 << candidate
                if selected & bit:
                    continue
                if not (adjacency[candidate] & reduced):
                    candidates.append(candidate)

            for i, first in enumerate(candidates):
                first_bit = 1 << first
                for second in candidates[i + 1 :]:
                    if not (adjacency[first] & (1 << second)):
                        selected = reduced | first_bit | (1 << second)
                        improved = True
                        break
                if improved:
                    break

            if improved:
                break

    return selected


def local_search_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
) -> list[int]:
    adjacency = build_adjacency_bitsets(node_count, edges)
    selected = 0
    for vertex in min_degree_greedy_independent_set(node_count, edges):
        selected |= 1 << vertex

    return bitset_to_sorted_list(
        improve_independent_set_1_2(node_count, adjacency, selected)
    )


def reducing_peeling_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
) -> list[int]:
    adjacency = build_adjacency_bitsets(node_count, edges)
    active = (1 << node_count) - 1
    selected = 0
    peeled_vertices = []

    while active:
        reduction_vertex = None
        reduction_degree = None
        current = active

        while current:
            bit = current & -current
            vertex = bit.bit_length() - 1
            degree = (adjacency[vertex] & active).bit_count()

            if degree <= 1 and (
                reduction_degree is None or degree < reduction_degree
            ):
                reduction_vertex = vertex
                reduction_degree = degree
                if degree == 0:
                    break

            current &= ~bit

        if reduction_vertex is not None:
            vertex_bit = 1 << reduction_vertex
            selected |= vertex_bit
            active &= ~(vertex_bit | adjacency[reduction_vertex])
            continue

        peel_vertex = None
        peel_degree = -1
        current = active

        while current:
            bit = current & -current
            vertex = bit.bit_length() - 1
            degree = (adjacency[vertex] & active).bit_count()
            if degree > peel_degree:
                peel_vertex = vertex
                peel_degree = degree
            current &= ~bit

        peeled_vertices.append(peel_vertex)
        active &= ~(1 << peel_vertex)

    for vertex in reversed(peeled_vertices):
        bit = 1 << vertex
        if not (selected & bit) and not (adjacency[vertex] & selected):
            selected |= bit

    selected = maximalize_independent_set(node_count, adjacency, selected)
    return bitset_to_sorted_list(selected)


def grasp_local_search_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
    iterations: int = GRASP_ITERATIONS,
    rcl_size: int = GRASP_RCL_SIZE,
    seed: int = RANDOM_SEED,
) -> list[int]:
    adjacency = build_adjacency_bitsets(node_count, edges)
    rng = random.Random(seed)
    best = 0

    for _ in range(iterations):
        active = (1 << node_count) - 1
        selected = 0

        while active:
            candidates = []
            current = active

            while current:
                bit = current & -current
                vertex = bit.bit_length() - 1
                degree = (adjacency[vertex] & active).bit_count()
                candidates.append((degree, vertex))
                current &= ~bit

            candidates.sort()
            _, vertex = rng.choice(candidates[: min(rcl_size, len(candidates))])
            selected |= 1 << vertex
            active &= ~((1 << vertex) | adjacency[vertex])

        selected = improve_independent_set_1_2(node_count, adjacency, selected)
        if selected.bit_count() > best.bit_count():
            best = selected

    return bitset_to_sorted_list(best)


def hybrid_iterated_local_search_independent_set(
    node_count: int,
    edges: list[tuple[int, int]],
    iterations: int = ILS_ITERATIONS,
    perturbation_fraction: float = ILS_PERTURBATION_FRACTION,
    seed: int = RANDOM_SEED,
) -> list[int]:
    adjacency = build_adjacency_bitsets(node_count, edges)
    rng = random.Random(seed)
    selected = 0

    for vertex in reducing_peeling_independent_set(node_count, edges):
        selected |= 1 << vertex
    selected = improve_independent_set_1_2(node_count, adjacency, selected)
    best = selected

    for _ in range(iterations):
        selected_vertices = bitset_to_sorted_list(selected)
        if not selected_vertices:
            candidate = 0
        else:
            remove_count = max(
                1,
                int(len(selected_vertices) * perturbation_fraction),
            )
            removed_vertices = rng.sample(
                selected_vertices,
                min(remove_count, len(selected_vertices)),
            )
            candidate = selected
            for vertex in removed_vertices:
                candidate &= ~(1 << vertex)

        order = list(range(node_count))
        rng.shuffle(order)
        candidate = maximalize_independent_set(
            node_count,
            adjacency,
            candidate,
            order,
        )
        candidate = improve_independent_set_1_2(node_count, adjacency, candidate)

        if candidate.bit_count() >= selected.bit_count() or rng.random() < 0.05:
            selected = candidate
        if selected.bit_count() > best.bit_count():
            best = selected

    return bitset_to_sorted_list(best)


def process_memory_snapshot() -> tuple[int, int]:
    """Return current and lifetime-peak resident memory in bytes."""
    if os.name == "nt":
        size_t = ctypes.c_size_t

        class ProcessMemoryCounters(ctypes.Structure):
            _fields_ = [
                ("cb", ctypes.c_ulong),
                ("PageFaultCount", ctypes.c_ulong),
                ("PeakWorkingSetSize", size_t),
                ("WorkingSetSize", size_t),
                ("QuotaPeakPagedPoolUsage", size_t),
                ("QuotaPagedPoolUsage", size_t),
                ("QuotaPeakNonPagedPoolUsage", size_t),
                ("QuotaNonPagedPoolUsage", size_t),
                ("PagefileUsage", size_t),
                ("PeakPagefileUsage", size_t),
            ]

        counters = ProcessMemoryCounters()
        counters.cb = ctypes.sizeof(counters)
        kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)
        psapi = ctypes.WinDLL("psapi", use_last_error=True)
        kernel32.GetCurrentProcess.restype = ctypes.c_void_p
        psapi.GetProcessMemoryInfo.argtypes = [
            ctypes.c_void_p,
            ctypes.POINTER(ProcessMemoryCounters),
            ctypes.c_ulong,
        ]
        psapi.GetProcessMemoryInfo.restype = ctypes.c_int
        process_handle = kernel32.GetCurrentProcess()
        if not psapi.GetProcessMemoryInfo(
            process_handle,
            ctypes.byref(counters),
            counters.cb,
        ):
            raise ctypes.WinError(ctypes.get_last_error())
        return counters.WorkingSetSize, counters.PeakWorkingSetSize

    try:
        import resource
    except ImportError:
        return 0, 0

    peak_rss = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    if sys.platform != "darwin":
        peak_rss *= 1024
    return int(peak_rss), int(peak_rss)


def execute_algorithm(
    algorithm_key: str,
    node_count: int,
    edges: list[tuple[int, int]],
    seed: int,
) -> list[int]:
    """Dispatch one benchmark algorithm without timing or memory instrumentation."""
    if algorithm_key == "misa":
        return misa_greedy_from_edges(node_count, edges)
    if algorithm_key == "min_degree":
        return min_degree_greedy_independent_set(node_count, edges)
    if algorithm_key == "max_degree":
        return max_degree_removal_independent_set(node_count, edges)
    if algorithm_key == "random_greedy":
        return random_greedy_independent_set(
            node_count,
            edges,
            starts=RANDOM_GREEDY_STARTS,
            seed=seed,
        )
    if algorithm_key == "vertex_support":
        return vertex_support_greedy_independent_set(node_count, edges)
    if algorithm_key == "luby":
        return luby_randomized_independent_set(node_count, edges, seed=seed)
    if algorithm_key == "smallest_last":
        return smallest_last_greedy_independent_set(node_count, edges)
    if algorithm_key == "reducing_peeling":
        return reducing_peeling_independent_set(node_count, edges)
    if algorithm_key == "local_search":
        return local_search_independent_set(node_count, edges)
    if algorithm_key == "grasp":
        return grasp_local_search_independent_set(
            node_count,
            edges,
            iterations=GRASP_ITERATIONS,
            rcl_size=GRASP_RCL_SIZE,
            seed=seed,
        )
    if algorithm_key == "ils":
        return hybrid_iterated_local_search_independent_set(
            node_count,
            edges,
            iterations=ILS_ITERATIONS,
            perturbation_fraction=ILS_PERTURBATION_FRACTION,
            seed=seed,
        )
    if algorithm_key == "cplex":
        return cplex_qubo_exact_independent_set(node_count, edges, seed=seed)
    if algorithm_key == "color_bnb":
        return branch_and_bound_independent_set(node_count, edges)
    if algorithm_key == "kernel_bnb":
        return kernelized_branch_and_bound_independent_set(node_count, edges)
    if algorithm_key == "brute_force":
        return brute_force_independent_set(node_count, edges)
    raise ValueError(f"Unknown algorithm key: {algorithm_key}")


def algorithm_worker(
    result_connection,
    algorithm_key: str,
    node_count: int,
    edges: list[tuple[int, int]],
    seed: int,
) -> None:
    """Run one clean algorithm call in an isolated process.

    This pass owns the reported runtime and returned independent set.  It is
    intentionally separate from ``memory_profile_worker``: a slow
    tracemalloc-instrumented repeat must never turn a valid clean result into a
    timeout.
    """
    try:
        start = time.perf_counter()
        independent_set = execute_algorithm(algorithm_key, node_count, edges, seed)
        elapsed_seconds = time.perf_counter() - start
        current_rss, peak_rss = process_memory_snapshot()
        result_connection.send(
            {
                "status": "OK",
                "independent_set": independent_set,
                "elapsed_seconds": elapsed_seconds,
                "process_rss_mib": current_rss / (1024 * 1024),
                "process_peak_rss_mib": peak_rss / (1024 * 1024),
                "reason": "",
            }
        )
    except AlgorithmSkipped as exc:
        result_connection.send(
            {
                "status": "SKIPPED",
                "reason": str(exc),
            }
        )
    except Exception as exc:
        result_connection.send(
            {
                "status": "ERROR",
                "reason": f"{type(exc).__name__}: {exc}",
                "traceback": traceback.format_exc(),
            }
        )
    finally:
        result_connection.close()


def memory_profile_worker(
    result_connection,
    algorithm_key: str,
    node_count: int,
    edges: list[tuple[int, int]],
    seed: int,
) -> None:
    """Repeat one deterministic call under tracemalloc in a separate worker.

    ``tracemalloc`` measures allocations attributable to the algorithm instead
    of the shared Python/interpreter RSS baseline.  A timeout or error here is
    deliberately local to profiling; ``measure_algorithm_subprocess`` retains
    the already-completed clean result.
    """
    try:
        tracemalloc.start()
        execute_algorithm(algorithm_key, node_count, edges, seed)
        _, traced_peak_bytes = tracemalloc.get_traced_memory()
        result_connection.send(
            {
                "status": "OK",
                "alloc_peak_kib": traced_peak_bytes / 1024,
                "reason": "",
            }
        )
    except Exception as exc:
        result_connection.send(
            {
                "status": "ERROR",
                "reason": f"{type(exc).__name__}: {exc}",
                "traceback": traceback.format_exc(),
            }
        )
    finally:
        if tracemalloc.is_tracing():
            tracemalloc.stop()
        result_connection.close()


def run_worker_subprocess(
    worker_target,
    algorithm_key: str,
    node_count: int,
    edges: list[tuple[int, int]],
    seed: int,
    timeout_seconds: float,
) -> dict:
    """Execute one worker and return its result without sharing timeout budgets."""
    context = mp.get_context("spawn")
    result_connection, worker_connection = context.Pipe(duplex=False)
    process = context.Process(
        target=worker_target,
        args=(worker_connection, algorithm_key, node_count, edges, seed),
    )
    wall_start = time.perf_counter()
    process.start()
    worker_connection.close()
    process.join(timeout_seconds)

    if process.is_alive():
        process.terminate()
        process.join(5)
        if process.is_alive():
            process.kill()
            process.join()
        wall_seconds = time.perf_counter() - wall_start
        result_connection.close()
        process.close()
        return {
            "status": "TIMEOUT",
            "reason": f"exceeded {timeout_seconds:.1f} seconds",
            "worker_wall_seconds": wall_seconds,
        }

    wall_seconds = time.perf_counter() - wall_start
    if result_connection.poll():
        result = result_connection.recv()
    else:
        result = {
            "status": "ERROR",
            "reason": f"worker exited without a result (exit code {process.exitcode})",
        }
    result_connection.close()
    process.close()
    result["worker_wall_seconds"] = wall_seconds
    return result


def measure_algorithm_subprocess(
    algorithm_key: str,
    node_count: int,
    edges: list[tuple[int, int]],
    seed: int,
    timeout_seconds: float = ALGORITHM_TIMEOUT_SECONDS,
) -> dict:
    """Measure a clean run and, separately, its peak Python allocation.

    The clean algorithm call receives ``timeout_seconds``.  If it succeeds,
    profiling receives its own ``MEMORY_PROFILE_MAX_SECONDS`` budget in a fresh
    process.  A profiling timeout reports ``alloc_peak_kib=None`` while
    preserving the valid clean independent set and runtime.
    """
    clean_result = run_worker_subprocess(
        algorithm_worker,
        algorithm_key,
        node_count,
        edges,
        seed,
        timeout_seconds,
    )
    if clean_result["status"] != "OK":
        return clean_result

    clean_wall_seconds = clean_result["worker_wall_seconds"]
    clean_result["alloc_peak_kib"] = None
    clean_result["memory_profile_status"] = "SKIPPED"
    clean_result["memory_profile_reason"] = "clean run exceeded profiling budget"
    if clean_result["elapsed_seconds"] <= MEMORY_PROFILE_MAX_SECONDS:
        profile_result = run_worker_subprocess(
            memory_profile_worker,
            algorithm_key,
            node_count,
            edges,
            seed,
            MEMORY_PROFILE_MAX_SECONDS,
        )
        clean_result["worker_wall_seconds"] = (
            clean_wall_seconds + profile_result["worker_wall_seconds"]
        )
        clean_result["memory_profile_status"] = profile_result["status"]
        clean_result["memory_profile_reason"] = profile_result.get("reason", "")
        if profile_result["status"] == "OK":
            clean_result["alloc_peak_kib"] = profile_result["alloc_peak_kib"]
    return clean_result


def format_decimal(value: float, digits: int) -> str:
    """Format a number for the comparison table with a comma as decimal separator."""
    return f"{value:.{digits}f}".replace(".", ",")


def format_table(headers: list[str], rows: list[list[str]]) -> str:
    widths = [
        max(len(str(row[index])) for row in [headers] + rows)
        for index in range(len(headers))
    ]

    def format_row(row: list[str]) -> str:
        return " | ".join(
            str(value).ljust(widths[index]) for index, value in enumerate(row)
        )

    separator = "-+-".join("-" * width for width in widths)
    return "\n".join([format_row(headers), separator] + [format_row(row) for row in rows])


def run_comparison() -> None:
    benchmark_wall_start = time.perf_counter()
    algorithms = [
        (
            "MISA Malatya Greedy",
            "Greedy",
            "No",
            "O(V+E+R+U log V)",
            "O(V+E)",
            "This work",
            "Proposed method; excluded from external audit",
            None,
            "misa",
        ),
        (
            "Minimum-Degree Greedy MIS",
            "Greedy",
            "No",
            "O(V^2) bitset operations",
            "O(V^2) bits",
            "[R1]",
            "Direct published selection rule",
            None,
            "min_degree",
        ),
        (
            "Vertex Support Algorithm (VSA)",
            "Greedy",
            "No",
            "O(V*(V+E))",
            "O(V^2) bits",
            "[R4]",
            "Direct VSA rule with deterministic tie-break",
            None,
            "vertex_support",
        ),
        (
            "Luby Random-Priority Maximal IS",
            "Randomized greedy",
            "No",
            "O((V+E) log V) exp.",
            "O(V^2) bits",
            "[R5]",
            "Published randomized rule; sequential simulation",
            None,
            "luby",
        ),
        (
            "CPLEX Exact (QUBO/MIQP)",
            "Exact",
            "Yes",
            "Worst-case exponential; exact MIQP",
            "O(V+E) model; solver dependent",
            "[R11, R12, R13]",
            "Exact QUBO formulation; CPLEX proof is required",
            CPLEX_EXACT_NODE_LIMIT,
            "cplex",
        ),
        (
            "Color-Bound B&B Exact",
            "Exact",
            "Yes",
            "Exponential; pruned search",
            "O(V^2)",
            "[R14]",
            "Tomita-style exact core; simplified implementation",
            COLOR_BNB_NODE_LIMIT,
            "color_bnb",
        ),
        (
            "Brute Force Backtracking Exact",
            "Exact",
            "Yes",
            "O(2^V)",
            "O(V^2) bits + O(V) stack",
            "[R13, R16]",
            "Naive include/exclude baseline; not the optimized R16 solver",
            BRUTE_FORCE_NODE_LIMIT,
            "brute_force",
        ),
    ]

    summaries = {
        algorithm: {
            "class": algorithm_class,
            "optimal": optimal,
            "theory_time": theory_time,
            "theory_memory": theory_memory,
            "reference": reference,
            "source_scope": source_scope,
            "sizes": [],
            "gaps": [],
            "ratios": [],
            "times": [],
            "worker_wall_times": [],
            "alloc_peak_kib": [],
            "valid": [],
            "status_counts": {
                "OK": 0,
                "SKIPPED": 0,
                "TIMEOUT": 0,
                "ERROR": 0,
            },
            "reasons": [],
        }
        for (
            algorithm,
            algorithm_class,
            optimal,
            theory_time,
            theory_memory,
            reference,
            source_scope,
            _,
            _,
        ) in algorithms
    }
    edge_counts = []
    seeds = list(GENERAL_SEEDS)
    reference_modes = []
    csv_fields = [
        "Seed",
        "Nodes",
        "Edges",
        "Algorithm",
        "Status",
        "Reason",
        "Algorithm Time (s)",
        "Worker Wall Time (s)",
        "Alloc Peak (KiB)",
        "Process RSS (MiB)",
        "Process Peak RSS (MiB)",
        "IS Size",
        "Valid",
    ]

    print(
        "Benchmark execution: one isolated clean process per algorithm call; "
        "eligible runs use a separate profiling process; "
        f"clean timeout={ALGORITHM_TIMEOUT_SECONDS:.1f}s."
    )
    print(f"Incremental results: {INCREMENTAL_RESULTS_CSV}")
    print(flush=True)

    with open(INCREMENTAL_RESULTS_CSV, "w", newline="", encoding="utf-8") as csv_file:
        csv_writer = csv.DictWriter(csv_file, fieldnames=csv_fields)
        csv_writer.writeheader()
        csv_file.flush()

        for graph_index, seed in enumerate(seeds, start=1):
            data = create_random_general_graph(
                GENERAL_NODE_COUNT,
                GENERAL_EDGE_PROBABILITY,
                seed,
            )
            node_count = data.graph.vcount()
            edge_count = len(data.edges)
            edge_counts.append(edge_count)
            graph_path = save_general_graph_txt(
                data,
                node_count,
                GENERAL_EDGE_PROBABILITY,
                seed,
            )
            seed_measurements = []
            print(
                f"[Graph {graph_index}/{len(seeds)}] seed={seed}, "
                f"nodes={node_count}, edges={edge_count}",
                flush=True,
            )
            print(f"  stored graph -> {graph_path}", flush=True)

            for (
                algorithm,
                _,
                optimal,
                _,
                _,
                _,
                _,
                max_nodes,
                algorithm_key,
            ) in algorithms:
                if max_nodes is not None and node_count > max_nodes:
                    measurement = {
                        "status": "SKIPPED",
                        "reason": f"n>{max_nodes}",
                        "worker_wall_seconds": None,
                    }
                else:
                    print(f"  START   {algorithm}", flush=True)
                    measurement = measure_algorithm_subprocess(
                        algorithm_key,
                        node_count,
                        data.edges,
                        seed,
                    )

                status = measurement["status"]
                reason = measurement.get("reason", "")
                independent_set = measurement.get("independent_set")
                valid = ""
                independent_set_size = ""

                if status == "OK":
                    valid = is_independent_set(data.edges, independent_set)
                    independent_set_size = len(independent_set)
                    if not valid:
                        status = "ERROR"
                        reason = "returned a non-independent vertex set"

                summary = summaries[algorithm]
                summary["status_counts"][status] += 1
                if reason:
                    summary["reasons"].append(f"seed={seed}: {reason}")

                csv_writer.writerow(
                    {
                        "Seed": seed,
                        "Nodes": node_count,
                        "Edges": edge_count,
                        "Algorithm": algorithm,
                        "Status": status,
                        "Reason": reason,
                        "Algorithm Time (s)": measurement.get("elapsed_seconds", ""),
                        "Worker Wall Time (s)": measurement.get(
                            "worker_wall_seconds",
                            "",
                        ),
                        "Alloc Peak (KiB)": measurement.get("alloc_peak_kib", ""),
                        "Process RSS (MiB)": measurement.get("process_rss_mib", ""),
                        "Process Peak RSS (MiB)": measurement.get(
                            "process_peak_rss_mib",
                            "",
                        ),
                        "IS Size": independent_set_size,
                        "Valid": valid,
                    }
                )
                csv_file.flush()

                if status == "OK":
                    alloc_peak_kib = measurement["alloc_peak_kib"]
                    alloc_text = (
                        f"{alloc_peak_kib:.2f}KiB"
                        if alloc_peak_kib is not None
                        else "n/a (long run)"
                    )
                    print(
                        f"  DONE    {algorithm}: "
                        f"time={measurement['elapsed_seconds']:.6f}s, "
                        f"wall={measurement['worker_wall_seconds']:.3f}s, "
                        f"alloc_peak={alloc_text}, "
                        f"proc_peak_rss={measurement['process_peak_rss_mib']:.2f}MiB, "
                        f"IS={independent_set_size}",
                        flush=True,
                    )
                    seed_measurements.append(
                        {
                            "algorithm": algorithm,
                            "optimal": optimal,
                            "size": independent_set_size,
                            "time": measurement["elapsed_seconds"],
                            "worker_wall": measurement["worker_wall_seconds"],
                            "alloc_peak_kib": measurement["alloc_peak_kib"],
                            "valid": valid,
                        }
                    )
                else:
                    print(
                        f"  {status:<7} {algorithm}: {reason}",
                        flush=True,
                    )
                    if status == "ERROR" and measurement.get("traceback"):
                        print(measurement["traceback"], flush=True)

            if not seed_measurements:
                reference_modes.append("unavailable")
                continue

            exact_sizes = [
                measurement["size"]
                for measurement in seed_measurements
                if measurement["optimal"] == "Yes"
            ]
            if exact_sizes:
                reference_size = max(exact_sizes)
                reference_modes.append("exact optimum")

                for measurement in seed_measurements:
                    if (
                        measurement["optimal"] == "Yes"
                        and measurement["size"] != reference_size
                    ):
                        raise RuntimeError(
                            f"{measurement['algorithm']} returned "
                            f"{measurement['size']}, but another exact algorithm "
                            f"returned {reference_size}."
                        )
            else:
                reference_size = max(
                    measurement["size"] for measurement in seed_measurements
                )
                reference_modes.append("best known")

            for measurement in seed_measurements:
                if measurement["size"] > reference_size:
                    raise RuntimeError(
                        f"{measurement['algorithm']} returned a larger independent "
                        "set than the exact reference. This indicates an "
                        "implementation error."
                    )

                summary = summaries[measurement["algorithm"]]
                summary["sizes"].append(measurement["size"])
                summary["gaps"].append(reference_size - measurement["size"])
                summary["ratios"].append(measurement["size"] / reference_size)
                summary["times"].append(measurement["time"])
                summary["worker_wall_times"].append(measurement["worker_wall"])
                if measurement["alloc_peak_kib"] is not None:
                    summary["alloc_peak_kib"].append(measurement["alloc_peak_kib"])
                summary["valid"].append(measurement["valid"])

    headers = [
        "Algorithm",
        "Class",
        "Optimal?",
        "Status",
        "Reference",
        "Implementation scope",
        "Runs",
        "Execution Time (s)",
        "Peak Alloc (KiB)",
        "Avg IS",
        "Avg Gap",
        "Hit Rate",
        "Avg Ratio",
        "Valid?",
    ]
    rows = []
    for algorithm, summary in summaries.items():
        run_count = len(summary["sizes"])
        runs = f"{run_count}/{len(seeds)}"
        status_parts = []
        for status in ("OK", "SKIPPED", "TIMEOUT", "ERROR"):
            count = summary["status_counts"][status]
            if count:
                status_parts.append(f"{status}:{count}")
        status_text = ", ".join(status_parts) or "NO RUN"

        if run_count == 0:
            rows.append(
                [
                    algorithm,
                    summary["class"],
                    summary["optimal"],
                    status_text,
                    summary["reference"],
                    summary["source_scope"],
                    runs,
                    "-",
                    "-",
                    "-",
                    "-",
                    "-",
                    "-",
                    "-",
                ]
            )
            continue

        rows.append(
            [
                algorithm,
                summary["class"],
                summary["optimal"],
                status_text,
                summary["reference"],
                summary["source_scope"],
                runs,
                format_decimal(sum(summary["times"]), 6),
                (
                    format_decimal(max(summary["alloc_peak_kib"]), 2)
                    if summary["alloc_peak_kib"]
                    else "n/a"
                ),
                format_decimal(sum(summary["sizes"]) / run_count, 2),
                format_decimal(sum(summary["gaps"]) / run_count, 2),
                format_decimal(
                    sum(1 for gap in summary["gaps"] if gap == 0) / run_count, 2
                ),
                format_decimal(sum(summary["ratios"]) / run_count, 4),
                str(all(summary["valid"])),
            ]
        )

    average_edges = sum(edge_counts) / len(edge_counts) if edge_counts else 0.0
    print(
        "General Erdos-Renyi graph tests: "
        f"n={GENERAL_NODE_COUNT}, p={GENERAL_EDGE_PROBABILITY}, "
        f"seeds={len(seeds)}, avg_edges={average_edges:.1f}"
    )
    print()
    print(format_table(headers, rows))
    print()
    if reference_modes and all(mode == "exact optimum" for mode in reference_modes):
        print("Avg Gap, Hit Rate and Avg Ratio are computed against the exact optimum.")
    elif any(mode == "best known" for mode in reference_modes):
        print(
            "Avg Gap, Hit Rate and Avg Ratio are computed against the best "
            "independent set found in each run because an exact reference "
            "was unavailable."
        )
    else:
        print("No successful run was available for quality comparisons.")
    print()
    print(
        "Execution Time is the sum of time measured around only the algorithm "
        "call in successful graph runs; for one graph it is the direct runtime. "
        "Peak Alloc (KiB) is the maximum heap allocation attributed to the "
        "algorithm itself by tracemalloc during a dedicated, un-timed profiling "
        "run; it isolates each algorithm's own data structures from the shared "
        "Python + igraph interpreter baseline, so it is the per-algorithm memory "
        "metric. tracemalloc only observes CPython heap allocations; the thirteen "
        "pure-Python algorithms build their own structures from the edge list, so "
        "their Peak Alloc is complete, but the CPLEX row (a native C++ solver) "
        "would report only its small Python-side model building, not the solver "
        "footprint. Clean runs longer than "
        f"{MEMORY_PROFILE_MAX_SECONDS:.0f}s skip profiling; a separate profiling "
        "run that exceeds that same limit also reports Peak Alloc as 'n/a'. "
        "Whole-process resident memory (Process RSS / Process Peak "
        "RSS) is dominated by the ~60+ MiB interpreter baseline and cannot rank "
        "these algorithms; it is retained only in the incremental CSV for "
        "diagnostics, alongside Worker Wall."
    )
    exact_run_count = sum(
        len(summary["sizes"])
        for summary in summaries.values()
        if summary["optimal"] == "Yes"
    )

    if exact_run_count:
        active_exact_algorithms = [
            algorithm
            for algorithm, summary in summaries.items()
            if summary["optimal"] == "Yes" and summary["sizes"]
        ]
        print(
            "Active exact references: " + ", ".join(active_exact_algorithms) + "."
        )
        print(
            "Every executed exact method must prove and return the same optimum. "
            "Local search and greedy methods remain heuristic."
        )
    else:
        print(
            "No exact algorithm was executed for this configuration. Reported "
            "gaps are against the best solution found by the executed heuristics, "
            "not against a proven optimum."
        )
    print(
        "Exact-method node limits: "
        f"CPLEX={CPLEX_EXACT_NODE_LIMIT}, "
        f"Color-B&B={COLOR_BNB_NODE_LIMIT}, "
        f"Reduced-B&B={KERNEL_BNB_NODE_LIMIT}, "
        f"Brute-Force={BRUTE_FORCE_NODE_LIMIT}."
    )
    print(f"Per-run timeout: {ALGORITHM_TIMEOUT_SECONDS:.1f} seconds.")
    print(f"Incremental results: {INCREMENTAL_RESULTS_CSV}")
    print(
        "Every generated random graph is stored as a text file in "
        f"'{GENERAL_GRAPH_DIR}' so the experiments can be repeated."
    )
    print(
        f"Total benchmark wall time: "
        f"{time.perf_counter() - benchmark_wall_start:.3f} seconds."
    )
    print()
    print("ACADEMIC REFERENCES FOR THE COMPARISON TABLE")
    print(
        "MISA Malatya Greedy is marked as 'This work'; R1-R16 audit all "
        "external comparison methods."
    )
    for reference_id, citation in ACADEMIC_REFERENCES:
        print(f"[{reference_id}] {citation}")


if __name__ == "__main__":
    mp.freeze_support()
    run_comparison()
