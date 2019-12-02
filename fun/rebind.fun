

inc x = x + 1
inc 42

qaz x = inc (inc x)
qaz 42

-- but this is broken...
inc x = inc (inc x)
inc 42
