import gleeunit
import gleeunit/should
import worker

pub fn main(){
  gleeunit.main()
}

pub fn test_sum_formula(){
  14
  |> should.equal(worker.consecutive_square_sum(1,3))
}

pub fn test_square_detection(){
  True |> should.equal(worker.check_square(25))
  False |> should.equal(worker.check_square(26))
}