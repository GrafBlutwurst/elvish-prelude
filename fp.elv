#Elvish Prelude for all sorts of FP inspired functions 

string-type = "string"
fn-type = "fn"
map-type = "map"
list-type = "list"

fn is-string [x]{ eq (kind-of $x $string-type) }
fn is-fn [x]{ eq (kind-of $x $fn-type) }
fn is-map [x]{ eq (kind-of $x $map-type) }
fn is-type [x]{ eq (kind-of $x $list-type) }


fn tryFn [f]{
  put [
    &eval= $f
    &flatMap= [g]{
      tryFn {
        $g ($f)
      }
    }
    &flatMapR= [g]{
      try {
        put ($f)
      } except e {
        $g e
      }
    }
    &fold= [onOk onErr]{
      tryFn {
        try {
          $onOk ($f)
        } except e {
          $onErr $e
        }
      }
    }
    &zip= [other]{
      (assume-map $other)[flatMap] [rhs]{
        [&l=($f) &r=($rhs[eval])]
      }
    }
  ]
}

fn success [value] {
  tryFn {
    put $value
  }
}

fn failure [err] {
  tryFn {
    fail $err
  }
}

fn assume-string [x]{
  tryFn {
    if (is-string $x) {
      put $x
    } else {
      fail $x" is not a String"
    }
  }
}

fn assume-fn [x]{
  tryFn {
    if (is-dn $x) {
      put $x
    } else {
      fail $x" is not a Function"
    }
  }
}

fn assume-map [x]{
  tryFn {
    if (is-map $x) {
      put $x
    } else {
      fail $x" is not a Map"
    }
  }
}

fn assume-list [x]{
  tryFn {
    if (is-list $x) {
      put $x
    } else {
      fail $x" is not a List"
    }
  }
}



fn fold-left [zero f list]{
  res = $zero
  each [x]{res = ($f $res $x)} $list
  put $res
}

fn filter [pred list]{
  each [x]{
    if ($pred $x) {
      put $x
    }
  } $list
}

fn contains [elem list]{
  not (eq (filter [x]{eq $x $elem} $list) [])
}

fn prependPut [elem list]{
  put $elem
  explode $list
}

fn prepend [elem list]{put [(prependPut $elem $list)]}

fn appendPut [list elem]{
  explode $list
  put $elem
}

fn append [list elem]{put [(appendPut $list $elem)]}

