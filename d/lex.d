import gram;

struct ScanStateTransition {
  uint			index;
  VecAction		live_diff;
  VecAction		accepts_diff;
}

struct ScanState {
  uint			index;
  ScanState 	*chars[256];
  VecAction		accepts;
  VecAction		live;
  ScanStateTransition	*transition[256];
}

extern(C) void build_scanners(Grammar *g);

