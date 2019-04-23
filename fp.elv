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
    &attempt= [g]{
      tryFn {
        $g ($f)
      }
    }
    &onError= [g]{
       tryFn {
        try {
          ($f)
        } except e {
          $g e
        }
       }
    }
    &flat-map= [other]{
      tryFn{
        $other[eval] $f
      }
    }
    &flat-map-e= [other]{
      tryFn{
        try {
          ($f)
        } except e {
          $other[eval] e
        }
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
      tryFn{
        put [&fst=$f &snd=$other[eval]]
      }
    }
  ]
}

fn success [value]{
  tryFn {
    put $value
  }
}

fn failure [err]{
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



fn fold-left [zero fun lst]{
  params = ((assume-fn $fun)[zip] (assume-list $lst))[attempt] [tpl]{
    put [&f= $tpl[fst] $list = $tpl[snd]]
  }
  
  $params[attempt] [rec]{
    res = $zero
    each [x]{res = ($rec[f] $res $x)} $rec[list]
    put $res
  }
  
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

