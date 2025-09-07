import gleam/list
import gleam/int
import gleam/erlang/process as process
import gleam/otp/actor
import worker
import messages.{type ControlMsg, InitializeSearch, PartialResult, WorkerFinished, Terminate, Analyze}

pub type ManagerState {
  Idle
  Running(pending: Int, results: List(Int), requester: process.Subject(List(Int)))
}

// Entry point to start the boss workflow
pub fn find_all_squares(limit: Int, length: Int, chunk_size: Int) -> Result(List(Int), String) {
  let assert Ok(manager_actor) =
    actor.new(Idle)
    |> actor.on_message(handle_manager)
    |> actor.start

  let requester = process.new_subject()
  let chunks = generate_chunks(limit, chunk_size)

  actor.send(manager_actor.data, InitializeSearch(limit, length, chunk_size, requester))

  chunks
  |>list.each(fn(chunk) {
  case chunk {
    ChunkRange(start, stop) -> {
      let assert Ok(worker_actor) = worker.initiate_worker()
      actor.send(worker_actor, Analyze(start, stop, length, manager_actor.data))
    }
  }
})


  case process.receive(requester, within: 60_000) {
    Ok(data) -> Ok(data)
    Error(Nil) -> Error("Timeout: no response from manager")
  }
}

// Handles manager state transitions
fn handle_manager(state: ManagerState, msg: ControlMsg) -> actor.Next(ManagerState, ControlMsg) {
  case state, msg {
    Idle, InitializeSearch(limit, _length, chunk_size, requester) -> {
      let expected = calculate_chunk_count(limit, chunk_size)
      Running(expected, [], requester) |> actor.continue
    }

    Running(pending, results, requester), PartialResult(found) -> {
      Running(pending, list.append(results, found), requester) |> actor.continue
    }

    Running(pending, results, requester), WorkerFinished -> {
      let remaining = pending - 1
      case remaining == 0 {
        True -> {
          process.send(requester, results)
          actor.stop()
        }
        False -> Running(remaining, results, requester) |> actor.continue
      }
    }

    _, Terminate -> actor.stop()
    _, _ -> actor.continue(state)
  }
}

// Chunk representation
pub type ChunkRange {
  ChunkRange(start: Int, end_: Int)
}

// Creates all the chunked ranges for scanning
fn generate_chunks(max: Int, size: Int) -> List(ChunkRange) {
  accumulate_chunks(1, [], max, size)
}

fn accumulate_chunks(start: Int, acc: List(ChunkRange), max: Int, size: Int) -> List(ChunkRange) {
  case start > max {
    True -> list.reverse(acc)
    False -> {
      let stop = int.min(start + size - 1, max)
      accumulate_chunks(start + size, [ChunkRange(start, stop), ..acc], max, size)

    }
  }
}

// Calculates the number of chunks required
fn calculate_chunk_count(max: Int, size: Int) -> Int {
  let assert Ok(q) = int.divide(max + size - 1, by: size)
  q
}
