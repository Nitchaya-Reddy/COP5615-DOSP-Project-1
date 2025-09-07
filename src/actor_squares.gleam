import gleam/io
import gleam/int
import gleam/list
import boss
import gleam/string

@external(erlang, "io", "get_line")
pub fn read_line(prompt: String) -> String

pub fn main() {
  // Read n
  let n_input = read_line("Enter n:") |> string.trim |> int.parse
  let n =
    case n_input {
      Ok(value) -> value
      Error(_) -> 0
    }

  // Read k
  let k_input = read_line("Enter k:") |> string.trim |> int.parse
  let k =
    case k_input {
      Ok(value) -> value
      Error(_) -> 0
    }

  // Compute consecutive squares sequences
  let result = boss.find_all_squares(n, k, 650)


  case result {
    Ok(starts) -> {
      io.println("Starting numbers of consecutive squares sequences:")

      let print_number = fn(s: Int) {
        io.println(int.to_string(s))
      }

      list.each(starts, print_number)
    }
    Error(msg) -> {
      io.println("Error: " <> msg)
    }
  }
}
