module dbus

data DBusTyKind = BasicTy | ContainerTy

mutual
  data DBusTy : DBusTyKind -> Type where
    DBusByte : DBusTy BasicTy
    DBusBoolean : DBusTy BasicTy
    DBusInt16 : DBusTy BasicTy
    DBusUInt16 : DBusTy BasicTy
    DBusInt32 : DBusTy BasicTy
    DBusUInt32 : DBusTy BasicTy
    DBusInt64 : DBusTy BasicTy
    DBusUInt64 : DBusTy BasicTy
    DBusDouble : DBusTy BasicTy
    DBusUnixFD : DBusTy BasicTy
    DBusString : DBusTy BasicTy
    DBusObjectPath : DBusTy BasicTy
    DBusSignature : DBusTy BasicTy
    DBusStruct : Signature -> DBusTy ContainerTy
    DBusArray : DBusTyAny -> DBusTy ContainerTy
    DBusVariant : DBusTy ContainerTy
    DBusDictEntry : DBusTy BasicTy -> DBusTyAny -> DBusTy ContainerTy

  DBusTyAny : Type
  DBusTyAny = (k : DBusTyKind ** DBusTy k)

  Signature : Type
  Signature = List DBusTyAny

ObjectPath : Type
ObjectPath = List String -- TODO: Each path element must not be empty

interpDBusTy : DBusTy k -> Type
interpDBusTy DBusByte = Bits8
interpDBusTy DBusBoolean = Bool
interpDBusTy DBusInt16 = ?interpDBusTy_rhs_3
interpDBusTy DBusUInt16 = Bits16
interpDBusTy DBusInt32 = ?interpDBusTy_rhs_5
interpDBusTy DBusUInt32 = Bits32
interpDBusTy DBusInt64 = ?interpDBusTy_rhs_7
interpDBusTy DBusUInt64 = Bits64
interpDBusTy DBusDouble = Float
interpDBusTy DBusUnixFD = ?interpDBusTy_rhs_10
interpDBusTy DBusString = String
interpDBusTy DBusObjectPath = ObjectPath
interpDBusTy DBusSignature = Signature
interpDBusTy (DBusStruct []) = ()
interpDBusTy (DBusStruct ((_ ** x) :: xs)) = (interpDBusTy x, interpDBusTy (DBusStruct xs))
interpDBusTy (DBusArray (_ ** x)) = List (interpDBusTy x)
interpDBusTy DBusVariant = ?interpDBusTy_rhs_16
interpDBusTy (DBusDictEntry x (_ ** y)) = (interpDBusTy x, interpDBusTy y)

parseDBusTy' : List Char -> (Maybe DBusTyAny, List Char)
parseDBusTy' ('y' :: xs) = (Just (_ ** DBusByte), xs)
parseDBusTy' ('b' :: xs) = (Just (_ ** DBusBoolean), xs)
parseDBusTy' ('n' :: xs) = (Just (_ ** DBusInt16), xs)
parseDBusTy' ('q' :: xs) = (Just (_ ** DBusUInt16), xs)
parseDBusTy' ('i' :: xs) = (Just (_ ** DBusInt32), xs)
parseDBusTy' ('u' :: xs) = (Just (_ ** DBusUInt32), xs)
parseDBusTy' ('x' :: xs) = (Just (_ ** DBusInt64), xs)
parseDBusTy' ('t' :: xs) = (Just (_ ** DBusUInt64), xs)
parseDBusTy' ('d' :: xs) = (Just (_ ** DBusDouble), xs)
parseDBusTy' ('h' :: xs) = (Just (_ ** DBusUnixFD), xs)
parseDBusTy' ('s' :: xs) = (Just (_ ** DBusString), xs)
parseDBusTy' ('g' :: xs) = (Just (_ ** DBusSignature), xs)
parseDBusTy' ('v' :: xs) = (Just (_ ** DBusVariant), xs)
parseDBusTy' ('a' :: xs) with (parseDBusTy' xs)
  parseDBusTy' ('a' :: xs) | (Just t, xs') = (Just (_ ** DBusArray t), xs')
  parseDBusTy' ('a' :: xs) | (Nothing, xs') = (Nothing, 'a' :: xs)
parseDBusTy' ('(' :: xs) = go xs
 where go1 : List Char -> (Maybe (List DBusTyAny), List Char)
       go1 (')' :: xs) = (Just [], xs)
       go1 [] = (Nothing, [])
       go1 xs with (parseDBusTy' xs)
         go1 xs | (Just t, xs') with (go1 xs')
           go1 xs | (Just t, _) | (Just ts, xs'') = (Just (t :: ts), xs'')
           go1 xs | (Just t, _) | (Nothing, _) = (Nothing, xs)
         go1 xs | (Nothing, _) = (Nothing, xs)
       go xs with (go1 xs)
         go xs | (Just ts, xs') = (Just (_ ** DBusStruct ts), xs')
         go xs | (Nothing, _) = (Nothing, '(' :: xs)
parseDBusTy' ('{' :: xs) with (parseDBusTy' xs)
  parseDBusTy' ('{' :: xs) | (Just (BasicTy ** t1), xs') with (parseDBusTy' xs')
    parseDBusTy' ('{' :: _) | (Just (BasicTy ** t1), _) | (Just t2, '}' :: xs'') = (Just (_ ** DBusDictEntry t1 t2), xs'')
    parseDBusTy' ('{' :: xs) | (Just _, _) | (_, _) = (Nothing, '{' :: xs)
  parseDBusTy' ('{' :: xs) | (_, _) = (Nothing, '{' :: xs)
parseDBusTy' xs = (Nothing, xs)
