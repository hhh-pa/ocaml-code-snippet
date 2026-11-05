// inscount: instruction-count tool for the ocaml snippet perf sweep.
//
// Counts every application instruction executed under Intel Pin and
// writes one line to the output file:
//
//	Count N
//
// The snip harness (ParsePinCount) reads the last such line. Counts
// are deterministic for a given binary + Pin version; compared runs
// must share one kit.
//
// Build out of the kit (PIN_ROOT = the extracted kit root, the
// directory holding the `pin` launcher):
//
//	make PIN_ROOT=<kit> -C <this dir>
//
// which drops obj-intel64/inscount.so next to this file.
//
// Instrumentation is per basic block: each block adds its static
// instruction count on entry, so the total equals executed
// instructions without paying for a call per instruction. The
// counter is atomic so multi-domain OCaml 5 binaries count exactly.
#include <atomic>
#include <cstdio>
#include <string>

#include "pin.H"

static std::atomic<unsigned long long> executed{0};
static FILE *sink = nullptr;
static KNOB<std::string> KnobOut(KNOB_MODE_WRITEONCE, "pintool", "o", "inscount.out",
                                 "file receiving the single Count line");

static VOID AddCount(UINT32 n) { executed.fetch_add(n, std::memory_order_relaxed); }

static VOID InstrumentTrace(TRACE trace, VOID *) {
    for (BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl)) {
        BBL_InsertCall(bbl, IPOINT_BEFORE, (AFUNPTR)AddCount, IARG_UINT32, BBL_NumIns(bbl),
                       IARG_END);
    }
}

static VOID Report(INT32, VOID *) {
    if (sink != nullptr) {
        std::fprintf(sink, "Count %llu\n", executed.load());
        std::fclose(sink);
        sink = nullptr;
    }
}

int main(int argc, char *argv[]) {
    PIN_InitSymbols();
    if (PIN_Init(argc, argv)) {
        return 1;
    }
    sink = std::fopen(KnobOut.Value().c_str(), "w");
    if (sink == nullptr) {
        return 1;
    }
    TRACE_AddInstrumentFunction(InstrumentTrace, nullptr);
    PIN_AddFiniFunction(Report, nullptr);
    PIN_StartProgram();
    return 0;
}
