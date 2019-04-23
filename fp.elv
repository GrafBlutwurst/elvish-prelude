#Elvish Prelude for all sorts of FP inspired functions 

string-type = "string"
fn-type = "fn"
map-type = "map"
list-type = "list"

fn is-string [x]{ eq (kind-of $x) $string-type }
fn is-fn [x]{ eq (kind-of $x) $fn-type }
fn is-map [x]{ eq (kind-of $x) $map-type }
fn is-list [x]{ eq (kind-of $x) $list-type }


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
          $f
        } except e {
          $g [&cause=$e[cause]]
        }
       }
    }
    &flat-map= [other]{
        $other ($f)
    }
    &fold= [onOk onErr]{
      tryFn {
        try {
          $onOk ($f)
        } except e {
          $onErr [&cause=$e[cause]]
        }
      }
    }
    &zip= [other]{
      tryFn {
        put [
          &fst=($f) 
          &snd=($other[eval])
        ]
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



fn prepend-put [elem list]{
  put $elem
  explode $list
}

fn prepend [elem list]{put [(prepend-put $elem $list)]}

fn append-put [list elem]{
  explode $list
  put $elem
}

fn append [list elem]{put [(append-put $list $elem)]}



fn assume-string [x]{
  tryFn {
    if (is-string $x) {
      put $x
    } else {
      fail ((to-string $x)" is not a String")
    }
  }
}

fn assume-fn [x]{
  tryFn {
    if (is-fn $x) {
      put $x
    } else {
      fail ((to-string $x)" is not a Function")
    }
  }
}

fn assume-map [x]{
  tryFn {
    if (is-map $x) {
      put $x
    } else {
      fail ((to-string $x)" is not a Map")
    }
  }
}

fn assume-list [x]{
  tryFn {
    if (is-list $x) {
      put $x
    } else {
      fail ((to-string $x)" is not a List")
    }
  }
}

fn empty [list]{eq $list []}

fn not-empty [list]{not-eq $list []}



fn fold-left-p [zero fun lst]{
  params = (((assume-fn $fun)[zip] (assume-list $lst))[attempt] [tpl]{
    put [
      &f= $tpl[fst] 
      &list= $tpl[snd]
    ]
  })
  
  $params[attempt] [rec]{
    res = $zero
    each [x]{res = ($rec[f] $res $x)} $rec[list]
    put $res
  }
}

fn fold-left [zero fun lst]{ (fold-left-p $zero $fun $lst)[eval] }

fn filter-p [pred list]{
  params = (((assume-fn $pred)[zip] (assume-list $list))[attempt] [tpl]{
    put [
      &pred= $tpl[fst]
      &list= $tpl[snd]
    ]
  })

  $params[flat-map] [rec]{
    fold-left-p [] [acc x]{
      if ($rec[pred] $x) {
        put (append $x  $acc)
      } else {
        put $acc
      }
    } $rec[list]
  }
}

fn filter [pred list]{ (filter-p $pred $list)[eval] }



fn contains-p [elem list]{
  (filter-p [x]{put (eq $x $elem)} $list)[attempt] [lst]{
    put (not-empty $lst)
  }
}

fn contains [elem list]{(contains-p $elem $list)[eval]}
