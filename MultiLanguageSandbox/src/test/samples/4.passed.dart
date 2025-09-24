void main() {
  String email = 'user@example.com';
  assert(email.contains('@') && email.endsWith('.com'), 'Invalid email format');
}
