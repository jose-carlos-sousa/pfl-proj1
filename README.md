# pfl-proj1




# Questão 1 

### Shortest Path Function Implementation

In the `shortestPath` function we used Dijkstra's algorithm to find all shortest paths between two cities in a `RoadMap`. This way, it efficiently explores paths between two given cities by prioritizing shorter routes using a priority queue.

#### Data Structures

1. **Adjacency List (`AdjList`)**:
   - The roadmap is converted to an `AdjList` with `roadMapToAdjList` to allow fast neighbour lookups.
   - **Justification**: The adjacency list is efficient for sparse graphs, saving space and enabling quick access to neighboring nodes.

2. **Priority Queue (`QueueEntry`)**:
   - Each entry in the queue is a tuple `(Distance, City, Path)`, ordered by cumulative distance.
   - **Justification**: The queue prioritizes shorter paths, ensuring cities are explored in order of distance.

#### Algorithm Steps

1. **Initialization**:
   - If `start == end`, returns `[start]`. Otherwise, it initializes the priority queue with `(0, start, [start])` and begins exploring paths.

2. **Main Loop (Dijkstra’s Algorithm)**:
   - The algorithm dequeues the entry with the shortest cumulative distance.
   - If the current city matches `end`, the path is added to the result if it has the minimum distance.
   - Neighbors are enqueued with updated distances, with paths maintaining order using `insertQueue`.
   - **Path Selection**: Only paths with the minimal total distance to `end` are retained.

#### Complexity

The algorithm achieves a time complexity of O((V + E) * V), where (V) is the number of cities and (E) the number of roads, due to the efficient use of an adjacency list and priority queue.

Note that this is not the most optimal time complexity for Dijkstra's algorithm, which can be O((V + E) * log V) when using a Fibonacci heap for the priority queue. However, we think the current implementation is sufficient for the project's requirements.
