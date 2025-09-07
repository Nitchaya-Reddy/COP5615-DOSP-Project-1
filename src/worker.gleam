import gleam/list
import gleam/int
import gleam/float
import gleam/otp/actor
import gleam/erlang/process as process
import gleam/result
import messages.{type TaskMsg, Analyze, HaltWorker, PartialResult, WorkerFinished}

// Launches a stateless actor to perform number range scanning
pub fn initiate_worker() -> Result(process.Subject(TaskMsg), actor.StartError) {
  actor.new(Nil)
  |> actor.on_message(process_incoming)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

// Handle messages sent to the worker
fn process_incoming(_state: Nil, message: TaskMsg) -> actor.Next(Nil, TaskMsg) {
  case message {
    Analyze(begin, finish, count, boss) -> {
      let findings = calculate_valid_starts(begin, finish, count)
      process.send(boss, PartialResult(findings))
      process.send(boss, WorkerFinished)
      actor.stop()
    }

    HaltWorker -> actor.stop()
  }
}

// Computes all valid starting numbers in the range
fn calculate_valid_starts(from: Int, to: Int, count: Int) -> List(Int) {
  evaluate(from, [], to, count)
}

fn evaluate(current: Int, collected: List(Int), limit: Int, length: Int) -> List(Int) {
  case current > limit {
    True -> list.reverse(collected)
    False -> {
      let total = consecutive_square_sum(current, length)
      let updated = case check_square(total) {
        True -> [current, ..collected]
        False -> collected
      }
      evaluate(current + 1, updated, limit, length)
    }
  }
}

// Determines if a number is a perfect square
pub fn check_square(num: Int) -> Bool {
  case num < 0 {
    True -> False
    False -> case int.square_root(num) {
      Ok(root) -> {
        let rounded = float.round(root)
        rounded * rounded == num
      }
      Error(_) -> False
    }
  }
}

// Uses a mathematical formula to compute the sum of `count` squares from `start`
pub fn consecutive_square_sum(start: Int, count: Int) -> Int {
  let term1 = count * start * start
  let term2 = count * {count - 1} * start
  let term3 = {count - 1} * count * {2 * count - 1} / 6
  term1 + term2 + term3
}
