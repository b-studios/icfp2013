sed -e \
  's/"and"/OpOp2 And/g;
  s/"or"/OpOp2 Or/g;
  s/"xor"/OpOp2 Xor/g;
  s/"plus"/OpOp2 Plus/g;
  s/"not"/OpOp1 Not/g;
  s/"shl1"/OpOp1 Shl1/g;
  s/"shr1"/OpOp1 Shr1/g;
  s/"shr4"/OpOp1 Shr4/g;
  s/"shr16"/OpOp1 Shr16/g;
  s/"if0"/OpIf0/g;
  s/"fold"/OpFold/g;
  s/"tfold"/OpTFold/g;
  s/"bonus", //g;' \
  ProblemsDB.hs
