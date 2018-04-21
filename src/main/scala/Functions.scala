object Functions {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  /* Exercise 2.3 page 27 */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  /* Exercise 2.4 page 27 */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  /* Exercise 2.5 page 27 */
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
