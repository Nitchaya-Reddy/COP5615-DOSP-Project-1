import gleam/erlang/process

// Instructions that can be sent to worker actors
pub type TaskMsg {
  Analyze(start_index: Int, end_index: Int, length: Int, supervisor: process.Subject(ControlMsg))
  HaltWorker
}

// Commands and reports that can be sent to the supervisor (boss)
pub type ControlMsg {
  InitializeSearch(range_end: Int, segment_size: Int, block_size: Int, responder: process.Subject(List(Int)))
  PartialResult(List(Int))
  WorkerFinished
  Terminate
}