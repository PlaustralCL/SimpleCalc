# Simple Calc
A simple calculator because I  wanted real division and was too lazy to type `bc -l` when using Linux. This program also demonstrates the power of Free Pascal and the Lazarus IDE to build cross-platform apps with small, native executables. 

## Installation
1. Download the most recent version for your platform from the releases page.
2. If you want to use it from anywhere, place the file somewhere in your Path.
3. On Linux, make the file executable, `chmod +x <filename>`

## Usage
- `help` - read this information
- `quit` - exit the program

### Constants
- `pi` - The value of Pi, 3.1415926535897932385
- `exp` - The value of Euler's number, 2.71828182845905

### Math operations:
  `@` The result of the previous calculation. Initially set to 0.
  `+` Addition
  `-` Subtraction
  `/` Real division. 10 / = 3.33333333333333
  `*` Multiplication
  `^` Exponentiation
  `%` Modulo. Returns the remainder after division. 10 % 3 = 1
  `\` Integer division. 10 \ 3 = 3

### Order of Operations
Order of operations follows the normal order in math or Python:
  1. parentheses
  2. exponents
  3. multiplication, division, integer division, and modulo
  4. addition and subtraction
### Notes
- There is no implicit multiplication. Use * for all multiplication.
- Where operators have the same precedence, they are evaluated from left to right.
- Whitespace does not impact the calculation.
- Negative number are processed as expected. However, you cannot negate a
  negative number back to positive. For example, --3 or -(-3) will throw an
  exception. Instead, multiply it by negative one: -1 * -3 = 3.