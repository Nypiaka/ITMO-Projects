# ITMO-Projects
Here are collected and will be collected the most complex and significant projects that were written by me during my studies at ITMO University.
<h2><strong>1. Scanner</strong></h2>
<p>
    <p>Implemented its analogue of the Scanner class based on Reader.</p>
    <p>Block reading is used. The code that controls reading is generic.</p>
    <p>Code that highlights numbers and common words.</p>
    <p>Added basic support for error handling and basic functions of the original Scanner.</p>
   <p>My scanner is significantly faster when reading large files.</p>
<h2><strong>2. Md to HTML parser</strong></h2>
<p>
    <p>A converter from Markdown to HTML has been developed.</p>
    <p>The converter supports the following features:</p>
    <p>Paragraphs of text are separated by blank lines.</p>
    <p>Inline elements: emphasis (* or _), strong emphasis (** or __), strikethrough (--), code (`)</p>
    <p>Headers (# * header level)</p>
    <p>Support for <<inserts>> and }}deletions{{</p>
    <p>Support for ```code __without__ formatting```</p>
    <p>Support for ''quotes''</p>
    <p>Support for %variables%</p>
<p>The converter takes two arguments: the name of the input file with Markdown markup and the name of the output file with HTML markup. Both files must be UTF-8 encoded.</p>
<h2><strong>3. MNK game</strong></h2>
<p>Implemented MNK game from Main. You can play both on a rectangular field M * N, and on the field M * M for playing a hex. Added error handling, the ability to surrender, or offer a draw.</p>
<h2><strong>4. Expression parser</strong></h2>
    <p> The expression is built on a record of the form </p>
   <p> x * (x - 2)*x + 1 </p>
 <p> Expressions can appear in the entry:</p>
 <p> binary operations: multiplication *, division /, addition + and subtraction -;</p>
  <p> unary minus -;</p>
<p> variables x, y and z;</p>
   <p> decimal integer constants that fit into a 32-bit signed integer type;</p>
      <p> parentheses to explicitly indicate operator precedence;</p>
       <p> an arbitrary number of whitespace characters in any place that does not affect the unambiguity of understanding the formula (for example, between an operation and a variable, but not inside constants). </p>
   <p>Priority of operations, starting with the highest</p>
      <p> unary minus;</p>
        <p>multiply and divide;</p>
        <p>Addition and subtraction.</p>
 <p> Expressions are parsed using the recursive descent method.</p>
    <p> The algorithm runs in linear time.</p>
      <p> Additionally implemented binary operations with minimum priority:</p>
  <p> << - shift left (1 << 5 + 3 equals 1 << (5 + 3) equals 256);</p>
   <p> >> shift right (1024 >> 5 + 3 equals 1024 >> (5 + 3) equals 4);</p>
   <p> >>> - arithmetic right shift (-1024 >>> 5 + 3 equals 1024 >>> (5 + 3) equals -4);</p>
   <p> An algorithm for optimizing bracket notation (the toMiniString method) has also been developed.</p>
