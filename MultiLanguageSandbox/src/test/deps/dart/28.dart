
String decipher(String encryptedMessage, String originalMessage, String toTranslate)
/*The countries of R and S are embroiled in war, with both sides dispatching spies to infiltrate the other, lying in wait for the opportunity to act. After enduring numerous hardships, spy C from R, who had been lurking in S, finally deciphered the encoding rules of S's military cipher:

1. The original messages intended for internal transmission within the military of S are encrypted before being sent over the network. Both the content of the original messages and the encrypted output consist solely of uppercase letters $\texttt{A}\sim\texttt{Z}$ (excluding spaces and other characters);
2. S has designated a specific cipher letter for each letter. The encryption process involves replacing every letter in the original message with its corresponding cipher letter;
3. Each letter corresponds to one unique cipher letter, and different letters correspond to different cipher letters. The cipher letter can be the same as the original letter.

For instance, if $\tt A$'s cipher letter is designated as $\tt A$, and $\tt B$'s as $\tt C$ (other letters and ciphers omitted), then the original message $\tt ABA$ is encrypted to $\tt ACA$.

Now, spy C has obtained one encrypted message sent over the network of S and its corresponding original message through an inside source. C hopes to use this information to crack S's military cipher. C's decryption process is as follows: scanning the original message, for the letter $x$ (representing any uppercase letter) in the original message, find its corresponding uppercase letter $y$ in the encrypted message, and consider $y$ as the cipher letter for $x$. This process continues until it stops at one of the following situations:

1. All information has been scanned, and all 26 letters from $\texttt{A}\sim\texttt{Z}$ have appeared in the original message and have been assigned corresponding cipher letters;
2. All information has been scanned, but it is found that some letter(s) did not appear in the original message;
3. During scanning, it is discovered that the information in hand contains obvious contradictions or errors (violating the encoding rules of S's cipher).

Input Format

There are three lines in total, each being a string of length between $1$ and $100$.
The first line is an encrypted message obtained by C;
The second line is the original message corresponding to the encrypted message in the first line;
The third line is the encrypted message that the command headquarters of R asks C to translate.

Output Format

A single line.
If decryption stops due to situations $2$ or $3$, output $\tt Failed$;
Otherwise, output the original message obtained by translating the encrypted message in the telegram using the deciphered cipher.

example
decipher("QWERTYUIOPLKJHGFDSAZXCVBNM", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "QWERTY")  return "ABCDEF"
*/
{
  if (encryptedMessage.length != originalMessage.length) {
    return "Failed";
  }

  Map<String, String> cipherMap = {};
  Map<String, String> reverseCipherMap = {};

  for (int i = 0; i < originalMessage.length; i++) {
    String originalChar = originalMessage[i];
    String encryptedChar = encryptedMessage[i];

    // Check if originalChar is already mapped to a different encryptedChar
    if (cipherMap.containsKey(originalChar)) {
      if (cipherMap[originalChar] != encryptedChar) {
        return "Failed";
      }
    } else {
      cipherMap[originalChar] = encryptedChar;
    }

    // Check if encryptedChar is already mapped to a different originalChar
    if (reverseCipherMap.containsKey(encryptedChar)) {
      if (reverseCipherMap[encryptedChar] != originalChar) {
        return "Failed";
      }
    } else {
      reverseCipherMap[encryptedChar] = originalChar;
    }
  }

  // Check if all 26 letters are present in the original message
  Set<String> allLetters = {'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
                           'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'};
  Set<String> originalLetters = originalMessage.split('').toSet();
  if (originalLetters.length != 26) {
    return "Failed";
  }

  // Decrypt the toTranslate message
  StringBuffer decryptedMessage = StringBuffer();
  for (int i = 0; i < toTranslate.length; i++) {
    String currentChar = toTranslate[i];
    if (!reverseCipherMap.containsKey(currentChar)) {
      return "Failed";
    }
    decryptedMessage.write(reverseCipherMap[currentChar]);
  }

  return decryptedMessage.toString();
}
void testDecipher() {
    // Test case with all letters mapped to themselves except the given ones are translated
  assert(decipher("QWERTYUIOPLKJHGFDSAZXCVBNM", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "QWERTY") == "ABCDEF");
  assert(decipher("QWERTYUIOPLKJHGFDSAZXCVBNM", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "HELLO") == "NCKKI");
  assert(decipher("MSRTZCJKPFLQYVAWBINXUEDGHOOILSMIJFRCOPPQCEUNYDUMPP", "YIZSDWAHLNOVFUCERKJXQMGTBPPKOIYKANZWPLLVWMQJFGQYLL", "FLSO") == "NOIP");
}

void main() {
  testDecipher();
}