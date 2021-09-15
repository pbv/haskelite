(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.t.U === region.u.U)
	{
		return 'on line ' + region.t.U;
	}
	return 'on lines ' + region.t.U + ' through ' + region.u.U;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aX,
		impl.a7,
		impl.a5,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		D: func(record.D),
		ag: record.ag,
		ad: record.ad
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.D;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.ag;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.ad) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aX,
		impl.a7,
		impl.a5,
		function(sendToApp, initialModel) {
			var view = impl.a8;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.aX,
		impl.a7,
		impl.a5,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.af && impl.af(sendToApp)
			var view = impl.a8;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.aP);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.a6) && (_VirtualDom_doc.title = title = doc.a6);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.a$;
	var onUrlRequest = impl.a0;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		af: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.aC === next.aC
							&& curr.ar === next.ar
							&& curr.az.a === next.az.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		aX: function(flags)
		{
			return A3(impl.aX, flags, _Browser_getUrl(), key);
		},
		a8: impl.a8,
		a7: impl.a7,
		a5: impl.a5
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { aV: 'hidden', aQ: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { aV: 'mozHidden', aQ: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { aV: 'msHidden', aQ: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { aV: 'webkitHidden', aQ: 'webkitvisibilitychange' }
		: { aV: 'hidden', aQ: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		aG: _Browser_getScene(),
		aJ: {
			aL: _Browser_window.pageXOffset,
			aM: _Browser_window.pageYOffset,
			aK: _Browser_doc.documentElement.clientWidth,
			ap: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		aK: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		ap: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			aG: {
				aK: node.scrollWidth,
				ap: node.scrollHeight
			},
			aJ: {
				aL: node.scrollLeft,
				aM: node.scrollTop,
				aK: node.clientWidth,
				ap: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			aG: _Browser_getScene(),
			aJ: {
				aL: x,
				aM: y,
				aK: _Browser_doc.documentElement.clientWidth,
				ap: _Browser_doc.documentElement.clientHeight
			},
			aT: {
				aL: x + rect.left,
				aM: y + rect.top,
				aK: rect.width,
				ap: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.e) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.h),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.h);
		} else {
			var treeLen = builder.e * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.i) : builder.i;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.e);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.h) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.h);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{i: nodeList, e: (len / $elm$core$Array$branchFactor) | 0, h: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {ao: fragment, ar: host, ax: path, az: port_, aC: protocol, aD: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$element = _Browser_element;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $author$project$Haskelite$Editing = 0;
var $author$project$AST$Fail = function (a) {
	return {$: 10, a: a};
};
var $author$project$Haskelite$Reducing = 1;
var $author$project$AST$Equation = F3(
	function (a, b, c) {
		return {$: 1, a: a, b: b, c: c};
	});
var $author$project$Pretty$operatorChar = function (c) {
	return (c === '+') || ((c === '*') || ((c === '-') || ((c === '>') || ((c === '<') || ((c === ':') || ((c === '=') || ((c === '&') || (c === '|'))))))));
};
var $author$project$Pretty$isOperator = $elm$core$String$all($author$project$Pretty$operatorChar);
var $author$project$Pretty$formatOperator = function (op) {
	return ((op === '&&') || (op === '||')) ? (' ' + (op + ' ')) : op;
};
var $author$project$Pretty$paren = F2(
	function (b, str) {
		return b ? ('(' + (str + ')')) : str;
	});
var $author$project$Pretty$prettyExpr = function (e) {
	return A2($author$project$Pretty$prettyExpr_, 0, e);
};
var $author$project$Pretty$prettyExpr_ = F2(
	function (prec, e) {
		switch (e.$) {
			case 3:
				var n = e.a;
				return $elm$core$String$fromInt(n);
			case 4:
				var b = e.a;
				return b ? 'True' : 'False';
			case 2:
				var x = e.a;
				return $author$project$Pretty$isOperator(x) ? ('(' + (x + ')')) : x;
			case 6:
				var l = e.a;
				return '[' + (A2(
					$elm$core$String$join,
					',',
					A2($elm$core$List$map, $author$project$Pretty$prettyExpr, l)) + ']');
			case 7:
				var l = e.a;
				return '(' + (A2(
					$elm$core$String$join,
					',',
					A2($elm$core$List$map, $author$project$Pretty$prettyExpr, l)) + ')');
			case 5:
				var e1 = e.a;
				var e2 = e.b;
				return A2(
					$author$project$Pretty$paren,
					prec > 0,
					A2($author$project$Pretty$prettyExpr_, 1, e1) + (':' + A2($author$project$Pretty$prettyExpr_, 1, e2)));
			case 8:
				var op = e.a;
				var e1 = e.b;
				var e2 = e.c;
				return A2(
					$author$project$Pretty$paren,
					prec > 0,
					_Utils_ap(
						A2($author$project$Pretty$prettyExpr_, 1, e1),
						_Utils_ap(
							$author$project$Pretty$formatOperator(op),
							A2($author$project$Pretty$prettyExpr_, 1, e2))));
			case 0:
				var e0 = e.a;
				var args = e.b;
				return A2(
					$author$project$Pretty$paren,
					prec > 0,
					A2(
						$elm$core$String$join,
						' ',
						A2(
							$elm$core$List$map,
							$author$project$Pretty$prettyExpr_(1),
							A2($elm$core$List$cons, e0, args))));
			case 1:
				var xs = e.a;
				var e1 = e.b;
				return A2(
					$author$project$Pretty$paren,
					prec > 0,
					'\\' + (A2($elm$core$String$join, ' ', xs) + (' -> ' + A2($author$project$Pretty$prettyExpr_, 1, e1))));
			case 9:
				var e1 = e.a;
				var e2 = e.b;
				var e3 = e.c;
				return A2(
					$author$project$Pretty$paren,
					prec > 0,
					'if ' + ($author$project$Pretty$prettyExpr(e1) + (' then ' + ($author$project$Pretty$prettyExpr(e2) + (' else ' + $author$project$Pretty$prettyExpr(e3))))));
			default:
				var msg = e.a;
				return msg;
		}
	});
var $author$project$Pretty$prettyPattern = function (p) {
	switch (p.$) {
		case 0:
			var x = p.a;
			return x;
		case 1:
			var b = p.a;
			return b ? 'True' : 'False';
		case 2:
			var n = p.a;
			return $elm$core$String$fromInt(n);
		case 5:
			var ps = p.a;
			return '(' + (A2(
				$elm$core$String$join,
				',',
				A2($elm$core$List$map, $author$project$Pretty$prettyPattern, ps)) + ')');
		case 3:
			var ps = p.a;
			return '[' + (A2(
				$elm$core$String$join,
				',',
				A2($elm$core$List$map, $author$project$Pretty$prettyPattern, ps)) + ']');
		default:
			var p1 = p.a;
			var p2 = p.b;
			return '(' + ($author$project$Pretty$prettyPattern(p1) + (':' + ($author$project$Pretty$prettyPattern(p2) + ')')));
	}
};
var $author$project$Pretty$prettyEquation = F3(
	function (f, ps, expr) {
		return A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$cons,
				f,
				A2($elm$core$List$map, $author$project$Pretty$prettyPattern, ps))) + (' = ' + $author$project$Pretty$prettyExpr(expr));
	});
var $author$project$Pretty$prettyInfix = F4(
	function (f, p1, p2, expr) {
		return $author$project$Pretty$prettyPattern(p1) + (' ' + (f + (' ' + ($author$project$Pretty$prettyPattern(p2) + (' = ' + $author$project$Pretty$prettyExpr(expr))))));
	});
var $author$project$Pretty$prettyDecl = function (decl) {
	if (!decl.$) {
		var f = decl.a;
		var str = decl.b;
		return f + (' :: ' + str);
	} else {
		var f = decl.a;
		var ps = decl.b;
		var expr = decl.c;
		if ((ps.b && ps.b.b) && (!ps.b.b.b)) {
			var p1 = ps.a;
			var _v2 = ps.b;
			var p2 = _v2.a;
			return $author$project$Pretty$isOperator(f) ? A4($author$project$Pretty$prettyInfix, f, p1, p2, expr) : A3($author$project$Pretty$prettyEquation, f, ps, expr);
		} else {
			return A3($author$project$Pretty$prettyEquation, f, ps, expr);
		}
	}
};
var $author$project$Eval$collectAlts = F2(
	function (fun, decls) {
		if (!decls.b) {
			return _Utils_Tuple2(_List_Nil, _List_Nil);
		} else {
			if (!decls.a.$) {
				var _v1 = decls.a;
				var rest = decls.b;
				return _Utils_Tuple2(_List_Nil, rest);
			} else {
				var _v2 = decls.a;
				var f = _v2.a;
				var ps = _v2.b;
				var e = _v2.c;
				var rest = decls.b;
				if (_Utils_eq(f, fun)) {
					var info = $author$project$Pretty$prettyDecl(
						A3($author$project$AST$Equation, f, ps, e));
					var _v3 = A2($author$project$Eval$collectAlts, fun, rest);
					var alts = _v3.a;
					var rest1 = _v3.b;
					return _Utils_Tuple2(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple3(ps, e, info),
							alts),
						rest1);
				} else {
					return _Utils_Tuple2(_List_Nil, decls);
				}
			}
		}
	});
var $author$project$AST$App = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $author$project$Eval$applyArgs = F2(
	function (e0, args) {
		if (!args.b) {
			return e0;
		} else {
			return A2($author$project$AST$App, e0, args);
		}
	});
var $author$project$AST$Cons = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $author$project$AST$IfThenElse = F3(
	function (a, b, c) {
		return {$: 9, a: a, b: b, c: c};
	});
var $author$project$AST$InfixOp = F3(
	function (a, b, c) {
		return {$: 8, a: a, b: b, c: c};
	});
var $author$project$AST$Lam = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $author$project$AST$ListLit = function (a) {
	return {$: 6, a: a};
};
var $author$project$AST$TupleLit = function (a) {
	return {$: 7, a: a};
};
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $author$project$AST$applySubst = F2(
	function (s, e) {
		switch (e.$) {
			case 3:
				return e;
			case 4:
				return e;
			case 2:
				var x = e.a;
				var _v1 = A2($elm$core$Dict$get, x, s);
				if (_v1.$ === 1) {
					return e;
				} else {
					var e1 = _v1.a;
					return e1;
				}
			case 1:
				var xs = e.a;
				var e1 = e.b;
				var s1 = A3($elm$core$List$foldr, $elm$core$Dict$remove, s, xs);
				return A2(
					$author$project$AST$Lam,
					xs,
					A2($author$project$AST$applySubst, s1, e1));
			case 0:
				var e1 = e.a;
				var args = e.b;
				return A2(
					$author$project$AST$App,
					A2($author$project$AST$applySubst, s, e1),
					A2(
						$elm$core$List$map,
						$author$project$AST$applySubst(s),
						args));
			case 5:
				var e1 = e.a;
				var e2 = e.b;
				return A2(
					$author$project$AST$Cons,
					A2($author$project$AST$applySubst, s, e1),
					A2($author$project$AST$applySubst, s, e2));
			case 8:
				var op = e.a;
				var e1 = e.b;
				var e2 = e.c;
				return A3(
					$author$project$AST$InfixOp,
					op,
					A2($author$project$AST$applySubst, s, e1),
					A2($author$project$AST$applySubst, s, e2));
			case 6:
				var l = e.a;
				return $author$project$AST$ListLit(
					A2(
						$elm$core$List$map,
						$author$project$AST$applySubst(s),
						l));
			case 7:
				var l = e.a;
				return $author$project$AST$TupleLit(
					A2(
						$elm$core$List$map,
						$author$project$AST$applySubst(s),
						l));
			case 9:
				var e1 = e.a;
				var e2 = e.b;
				var e3 = e.c;
				return A3(
					$author$project$AST$IfThenElse,
					A2($author$project$AST$applySubst, s, e1),
					A2($author$project$AST$applySubst, s, e2),
					A2($author$project$AST$applySubst, s, e3));
			default:
				return e;
		}
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$AST$ListP = function (a) {
	return {$: 3, a: a};
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $author$project$Eval$forceEval = F2(
	function (p, e) {
		var _v0 = _Utils_Tuple2(p, e);
		_v0$10:
		while (true) {
			switch (_v0.a.$) {
				case 0:
					return false;
				case 2:
					if (_v0.b.$ === 3) {
						return false;
					} else {
						break _v0$10;
					}
				case 1:
					if (_v0.b.$ === 4) {
						return false;
					} else {
						break _v0$10;
					}
				case 5:
					if (_v0.b.$ === 7) {
						var ps = _v0.a.a;
						var es = _v0.b.a;
						return A2($author$project$Eval$forceEvalList, ps, es);
					} else {
						break _v0$10;
					}
				case 3:
					switch (_v0.b.$) {
						case 6:
							var ps = _v0.a.a;
							var es = _v0.b.a;
							return A2($author$project$Eval$forceEvalList, ps, es);
						case 5:
							if (!_v0.a.a.b) {
								var _v1 = _v0.b;
								return false;
							} else {
								var _v2 = _v0.a.a;
								var p1 = _v2.a;
								var ps = _v2.b;
								var _v3 = _v0.b;
								var e1 = _v3.a;
								var e2 = _v3.b;
								return A2($author$project$Eval$forceEval, p1, e1) || A2(
									$author$project$Eval$forceEval,
									$author$project$AST$ListP(ps),
									e2);
							}
						default:
							break _v0$10;
					}
				default:
					switch (_v0.b.$) {
						case 5:
							var _v4 = _v0.a;
							var p1 = _v4.a;
							var p2 = _v4.b;
							var _v5 = _v0.b;
							var e1 = _v5.a;
							var e2 = _v5.b;
							return A2($author$project$Eval$forceEval, p1, e1) || A2($author$project$Eval$forceEval, p2, e2);
						case 6:
							if (!_v0.b.a.b) {
								var _v6 = _v0.a;
								var p1 = _v6.a;
								var p2 = _v6.b;
								return false;
							} else {
								var _v7 = _v0.a;
								var p1 = _v7.a;
								var p2 = _v7.b;
								var _v8 = _v0.b.a;
								var e1 = _v8.a;
								var rest = _v8.b;
								return A2($author$project$Eval$forceEval, p1, e1) || A2(
									$author$project$Eval$forceEval,
									p2,
									$author$project$AST$ListLit(rest));
							}
						default:
							break _v0$10;
					}
			}
		}
		return true;
	});
var $author$project$Eval$forceEvalList = F2(
	function (ps, es) {
		return A2(
			$elm$core$List$any,
			$elm$core$Basics$identity,
			A3($elm$core$List$map2, $author$project$Eval$forceEval, ps, es));
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $author$project$Eval$matching = F3(
	function (p, e, s) {
		switch (p.$) {
			case 0:
				var x = p.a;
				return $elm$core$Maybe$Just(
					A3($elm$core$Dict$insert, x, e, s));
			case 2:
				var n = p.a;
				if (e.$ === 3) {
					var m = e.a;
					return _Utils_eq(n, m) ? $elm$core$Maybe$Just(s) : $elm$core$Maybe$Nothing;
				} else {
					return $elm$core$Maybe$Nothing;
				}
			case 1:
				var b = p.a;
				if (e.$ === 4) {
					var c = e.a;
					return _Utils_eq(b, c) ? $elm$core$Maybe$Just(s) : $elm$core$Maybe$Nothing;
				} else {
					return $elm$core$Maybe$Nothing;
				}
			case 3:
				if (!p.a.b) {
					if ((e.$ === 6) && (!e.a.b)) {
						return $elm$core$Maybe$Just(s);
					} else {
						return $elm$core$Maybe$Nothing;
					}
				} else {
					var _v7 = p.a;
					var p1 = _v7.a;
					var ps = _v7.b;
					_v8$2:
					while (true) {
						switch (e.$) {
							case 6:
								if (e.a.b) {
									var _v9 = e.a;
									var e1 = _v9.a;
									var es = _v9.b;
									return A2(
										$elm$core$Maybe$andThen,
										A2(
											$author$project$Eval$matching,
											$author$project$AST$ListP(ps),
											$author$project$AST$ListLit(es)),
										A3($author$project$Eval$matching, p1, e1, s));
								} else {
									break _v8$2;
								}
							case 5:
								var e1 = e.a;
								var e2 = e.b;
								return A2(
									$elm$core$Maybe$andThen,
									A2(
										$author$project$Eval$matching,
										$author$project$AST$ListP(ps),
										e2),
									A3($author$project$Eval$matching, p1, e1, s));
							default:
								break _v8$2;
						}
					}
					return $elm$core$Maybe$Nothing;
				}
			case 5:
				var ps = p.a;
				if (e.$ === 7) {
					var es = e.a;
					return A3($author$project$Eval$matchingList, ps, es, s);
				} else {
					return $elm$core$Maybe$Nothing;
				}
			default:
				var p1 = p.a;
				var p2 = p.b;
				_v11$2:
				while (true) {
					switch (e.$) {
						case 5:
							var e1 = e.a;
							var e2 = e.b;
							return A2(
								$elm$core$Maybe$andThen,
								A2($author$project$Eval$matching, p2, e2),
								A3($author$project$Eval$matching, p1, e1, s));
						case 6:
							if (e.a.b) {
								var _v12 = e.a;
								var e1 = _v12.a;
								var e2 = _v12.b;
								return A2(
									$elm$core$Maybe$andThen,
									A2(
										$author$project$Eval$matching,
										p2,
										$author$project$AST$ListLit(e2)),
									A3($author$project$Eval$matching, p1, e1, s));
							} else {
								break _v11$2;
							}
						default:
							break _v11$2;
					}
				}
				return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Eval$matchingList = F3(
	function (ps, es, s) {
		var _v0 = _Utils_Tuple2(ps, es);
		_v0$2:
		while (true) {
			if (_v0.a.b) {
				if (_v0.b.b) {
					var _v1 = _v0.a;
					var p1 = _v1.a;
					var ps1 = _v1.b;
					var _v2 = _v0.b;
					var e1 = _v2.a;
					var es1 = _v2.b;
					return A2(
						$elm$core$Maybe$andThen,
						function (s1) {
							return A3($author$project$Eval$matchingList, ps1, es1, s1);
						},
						A3($author$project$Eval$matching, p1, e1, s));
				} else {
					break _v0$2;
				}
			} else {
				if (!_v0.b.b) {
					return $elm$core$Maybe$Just(s);
				} else {
					break _v0$2;
				}
			}
		}
		return $elm$core$Maybe$Nothing;
	});
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $author$project$Eval$dispatchAlts = F2(
	function (alts, args) {
		dispatchAlts:
		while (true) {
			if (!alts.b) {
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(
						$author$project$AST$Fail('pattern match failure'),
						'error'));
			} else {
				var _v1 = alts.a;
				var ps = _v1.a;
				var e = _v1.b;
				var info = _v1.c;
				var alts1 = alts.b;
				var nps = $elm$core$List$length(ps);
				var nargs = $elm$core$List$length(args);
				if (_Utils_cmp(nargs, nps) < 0) {
					return $elm$core$Maybe$Nothing;
				} else {
					if (A2($author$project$Eval$forceEvalList, ps, args)) {
						return $elm$core$Maybe$Nothing;
					} else {
						var args2 = A2($elm$core$List$drop, nps, args);
						var args1 = A2($elm$core$List$take, nps, args);
						var _v2 = A3($author$project$Eval$matchingList, ps, args1, $elm$core$Dict$empty);
						if (_v2.$ === 1) {
							var $temp$alts = alts1,
								$temp$args = args;
							alts = $temp$alts;
							args = $temp$args;
							continue dispatchAlts;
						} else {
							var s = _v2.a;
							var ne = A2(
								$author$project$Eval$applyArgs,
								A2($author$project$AST$applySubst, s, e),
								args2);
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(ne, info));
						}
					}
				}
			}
		}
	});
var $author$project$Eval$collectFunctions = F2(
	function (decls, accum) {
		collectFunctions:
		while (true) {
			if (!decls.b) {
				return accum;
			} else {
				if (!decls.a.$) {
					var _v1 = decls.a;
					var rest = decls.b;
					var $temp$decls = rest,
						$temp$accum = accum;
					decls = $temp$decls;
					accum = $temp$accum;
					continue collectFunctions;
				} else {
					var _v2 = decls.a;
					var fun = _v2.a;
					var ps = _v2.b;
					var e = _v2.c;
					var rest = decls.b;
					var info = $author$project$Pretty$prettyDecl(
						A3($author$project$AST$Equation, fun, ps, e));
					var _v3 = A2($author$project$Eval$collectAlts, fun, rest);
					var alts1 = _v3.a;
					var rest1 = _v3.b;
					var semantic = $author$project$Eval$dispatchAlts(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple3(ps, e, info),
							alts1));
					var accum1 = A3($elm$core$Dict$insert, fun, semantic, accum);
					var $temp$decls = rest1,
						$temp$accum = accum1;
					decls = $temp$decls;
					accum = $temp$accum;
					continue collectFunctions;
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$groupWhile = F2(
	function (isSameGroup, items) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					if (!acc.b) {
						return _List_fromArray(
							[
								_Utils_Tuple2(x, _List_Nil)
							]);
					} else {
						var _v1 = acc.a;
						var y = _v1.a;
						var restOfGroup = _v1.b;
						var groups = acc.b;
						return A2(isSameGroup, x, y) ? A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								x,
								A2($elm$core$List$cons, y, restOfGroup)),
							groups) : A2(
							$elm$core$List$cons,
							_Utils_Tuple2(x, _List_Nil),
							acc);
					}
				}),
			_List_Nil,
			items);
	});
var $author$project$Pretty$problemToString = function (prob) {
	switch (prob.$) {
		case 0:
			var s = prob.a;
			return s;
		case 1:
			return 'integer';
		case 7:
			return 'variable';
		case 8:
			var s = prob.a;
			return s;
		case 9:
			var s = prob.a;
			return s;
		case 10:
			return 'end of input';
		case 12:
			var s = prob.a;
			return s;
		default:
			return '?';
	}
};
var $author$project$Pretty$deadEndsToString = function (deadEnds) {
	var groups = A2(
		$elm_community$list_extra$List$Extra$groupWhile,
		F2(
			function (a, b) {
				return _Utils_eq(a.ae, b.ae) && _Utils_eq(a._, b._);
			}),
		deadEnds);
	return A2(
		$elm$core$String$join,
		'; ',
		A2(
			$elm$core$List$map,
			function (_v0) {
				var a = _v0.a;
				var r = _v0.b;
				return 'line ' + ($elm$core$String$fromInt(a.ae) + (',' + ('col ' + ($elm$core$String$fromInt(a._) + (': ' + ('expecting ' + A2(
					$elm$core$String$join,
					', ',
					A2(
						$elm$core$List$map,
						function (d) {
							return $author$project$Pretty$problemToString(d.a2);
						},
						A2($elm$core$List$cons, a, r)))))))));
			},
			groups));
};
var $elm$parser$Parser$Mandatory = 2;
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$Parser = $elm$core$Basics$identity;
var $elm$parser$Parser$Advanced$backtrackable = function (_v0) {
	var parse = _v0;
	return function (s0) {
		var _v1 = parse(s0);
		if (_v1.$ === 1) {
			var x = _v1.b;
			return A2($elm$parser$Parser$Advanced$Bad, false, x);
		} else {
			var a = _v1.b;
			var s1 = _v1.c;
			return A3($elm$parser$Parser$Advanced$Good, false, a, s1);
		}
	};
};
var $elm$parser$Parser$backtrackable = $elm$parser$Parser$Advanced$backtrackable;
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0;
		var parseB = _v1;
		return function (s0) {
			var _v2 = parseA(s0);
			if (_v2.$ === 1) {
				var p = _v2.a;
				var x = _v2.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v2.a;
				var a = _v2.b;
				var s1 = _v2.c;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
					var p2 = _v3.a;
					var x = _v3.b;
					return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _v3.a;
					var b = _v3.b;
					var s2 = _v3.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p1 || p2,
						A2(func, a, b),
						s2);
				}
			}
		};
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0;
		return function (s0) {
			var _v1 = parseA(s0);
			if (_v1.$ === 1) {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				var _v2 = callback(a);
				var parseB = _v2;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
					var p2 = _v3.a;
					var x = _v3.b;
					return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _v3.a;
					var b = _v3.b;
					var s2 = _v3.c;
					return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
				}
			}
		};
	});
var $elm$parser$Parser$andThen = $elm$parser$Parser$Advanced$andThen;
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.a);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.b, offset) < 0,
					0,
					{_: col, c: s0.c, d: s0.d, b: offset, ae: row, a: s0.a});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return function (s) {
		return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.b, s.ae, s._, s);
	};
};
var $elm$parser$Parser$chompWhile = $elm$parser$Parser$Advanced$chompWhile;
var $elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (_v1.$ === 1) {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					A2(
						func,
						A3($elm$core$String$slice, s0.b, s1.b, s0.a),
						a),
					s1);
			}
		};
	});
var $elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
};
var $elm$parser$Parser$getChompedString = $elm$parser$Parser$Advanced$getChompedString;
var $elm$parser$Parser$Problem = function (a) {
	return {$: 12, a: a};
};
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {_: col, aR: contextStack, a2: problem, ae: row};
	});
var $elm$parser$Parser$Advanced$Empty = {$: 0};
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.ae, s._, x, s.c));
	});
var $elm$parser$Parser$Advanced$problem = function (x) {
	return function (s) {
		return A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var $elm$parser$Parser$problem = function (msg) {
	return $elm$parser$Parser$Advanced$problem(
		$elm$parser$Parser$Problem(msg));
};
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, a, s);
	};
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $author$project$HsParser$infixOperator = A2(
	$elm$parser$Parser$andThen,
	function (s) {
		return $elm$core$String$isEmpty(s) ? $elm$parser$Parser$problem('operator') : $elm$parser$Parser$succeed(s);
	},
	$elm$parser$Parser$getChompedString(
		$elm$parser$Parser$chompWhile($author$project$Pretty$operatorChar)));
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
var $author$project$HsParser$operator = function (s) {
	return $elm$parser$Parser$backtrackable(
		A2(
			$elm$parser$Parser$andThen,
			function (r) {
				return _Utils_eq(s, r) ? $elm$parser$Parser$succeed(0) : $elm$parser$Parser$problem('operator ' + s);
			},
			$elm$parser$Parser$getChompedString(
				$elm$parser$Parser$chompWhile($author$project$Pretty$operatorChar))));
};
var $author$project$AST$BooleanP = function (a) {
	return {$: 1, a: a};
};
var $author$project$AST$ConsP = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$parser$Parser$Forbidden = 0;
var $author$project$AST$NumberP = function (a) {
	return {$: 2, a: a};
};
var $author$project$AST$VarP = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $author$project$HsParser$reservedWords = $elm$core$Set$fromList(
	_List_fromArray(
		['if', 'then', 'else', 'let', 'in', 'case', 'of', 'where']));
var $elm$parser$Parser$ExpectingVariable = {$: 7};
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm$parser$Parser$Advanced$varHelp = F7(
	function (isGood, offset, row, col, src, indent, context) {
		varHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, src);
			if (_Utils_eq(newOffset, -1)) {
				return {_: col, c: context, d: indent, b: offset, ae: row, a: src};
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$src = src,
						$temp$indent = indent,
						$temp$context = context;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					context = $temp$context;
					continue varHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$src = src,
						$temp$indent = indent,
						$temp$context = context;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					src = $temp$src;
					indent = $temp$indent;
					context = $temp$context;
					continue varHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$variable = function (i) {
	return function (s) {
		var firstOffset = A3($elm$parser$Parser$Advanced$isSubChar, i.t, s.b, s.a);
		if (_Utils_eq(firstOffset, -1)) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, i.am));
		} else {
			var s1 = _Utils_eq(firstOffset, -2) ? A7($elm$parser$Parser$Advanced$varHelp, i.aY, s.b + 1, s.ae + 1, 1, s.a, s.d, s.c) : A7($elm$parser$Parser$Advanced$varHelp, i.aY, firstOffset, s.ae, s._ + 1, s.a, s.d, s.c);
			var name = A3($elm$core$String$slice, s.b, s1.b, s.a);
			return A2($elm$core$Set$member, name, i.a3) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, i.am)) : A3($elm$parser$Parser$Advanced$Good, true, name, s1);
		}
	};
};
var $elm$parser$Parser$variable = function (i) {
	return $elm$parser$Parser$Advanced$variable(
		{am: $elm$parser$Parser$ExpectingVariable, aY: i.aY, a3: i.a3, t: i.t});
};
var $author$project$HsParser$identifier = $elm$parser$Parser$variable(
	{
		aY: function (c) {
			return $elm$core$Char$isAlphaNum(c) || ((c === '_') || (c === '\''));
		},
		a3: $author$project$HsParser$reservedWords,
		t: function (c) {
			return $elm$core$Char$isLower(c) || (c === '_');
		}
	});
var $elm$parser$Parser$ExpectingInt = {$: 1};
var $elm$parser$Parser$Advanced$consumeBase = _Parser_consumeBase;
var $elm$parser$Parser$Advanced$consumeBase16 = _Parser_consumeBase16;
var $elm$parser$Parser$Advanced$bumpOffset = F2(
	function (newOffset, s) {
		return {_: s._ + (newOffset - s.b), c: s.c, d: s.d, b: newOffset, ae: s.ae, a: s.a};
	});
var $elm$parser$Parser$Advanced$chompBase10 = _Parser_chompBase10;
var $elm$parser$Parser$Advanced$isAsciiCode = _Parser_isAsciiCode;
var $elm$parser$Parser$Advanced$consumeExp = F2(
	function (offset, src) {
		if (A3($elm$parser$Parser$Advanced$isAsciiCode, 101, offset, src) || A3($elm$parser$Parser$Advanced$isAsciiCode, 69, offset, src)) {
			var eOffset = offset + 1;
			var expOffset = (A3($elm$parser$Parser$Advanced$isAsciiCode, 43, eOffset, src) || A3($elm$parser$Parser$Advanced$isAsciiCode, 45, eOffset, src)) ? (eOffset + 1) : eOffset;
			var newOffset = A2($elm$parser$Parser$Advanced$chompBase10, expOffset, src);
			return _Utils_eq(expOffset, newOffset) ? (-newOffset) : newOffset;
		} else {
			return offset;
		}
	});
var $elm$parser$Parser$Advanced$consumeDotAndExp = F2(
	function (offset, src) {
		return A3($elm$parser$Parser$Advanced$isAsciiCode, 46, offset, src) ? A2(
			$elm$parser$Parser$Advanced$consumeExp,
			A2($elm$parser$Parser$Advanced$chompBase10, offset + 1, src),
			src) : A2($elm$parser$Parser$Advanced$consumeExp, offset, src);
	});
var $elm$parser$Parser$Advanced$finalizeInt = F5(
	function (invalid, handler, startOffset, _v0, s) {
		var endOffset = _v0.a;
		var n = _v0.b;
		if (handler.$ === 1) {
			var x = handler.a;
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				true,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		} else {
			var toValue = handler.a;
			return _Utils_eq(startOffset, endOffset) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				_Utils_cmp(s.b, startOffset) < 0,
				A2($elm$parser$Parser$Advanced$fromState, s, invalid)) : A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				toValue(n),
				A2($elm$parser$Parser$Advanced$bumpOffset, endOffset, s));
		}
	});
var $elm$parser$Parser$Advanced$fromInfo = F4(
	function (row, col, x, context) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, row, col, x, context));
	});
var $elm$core$String$toFloat = _String_toFloat;
var $elm$parser$Parser$Advanced$finalizeFloat = F6(
	function (invalid, expecting, intSettings, floatSettings, intPair, s) {
		var intOffset = intPair.a;
		var floatOffset = A2($elm$parser$Parser$Advanced$consumeDotAndExp, intOffset, s.a);
		if (floatOffset < 0) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				true,
				A4($elm$parser$Parser$Advanced$fromInfo, s.ae, s._ - (floatOffset + s.b), invalid, s.c));
		} else {
			if (_Utils_eq(s.b, floatOffset)) {
				return A2(
					$elm$parser$Parser$Advanced$Bad,
					false,
					A2($elm$parser$Parser$Advanced$fromState, s, expecting));
			} else {
				if (_Utils_eq(intOffset, floatOffset)) {
					return A5($elm$parser$Parser$Advanced$finalizeInt, invalid, intSettings, s.b, intPair, s);
				} else {
					if (floatSettings.$ === 1) {
						var x = floatSettings.a;
						return A2(
							$elm$parser$Parser$Advanced$Bad,
							true,
							A2($elm$parser$Parser$Advanced$fromState, s, invalid));
					} else {
						var toValue = floatSettings.a;
						var _v1 = $elm$core$String$toFloat(
							A3($elm$core$String$slice, s.b, floatOffset, s.a));
						if (_v1.$ === 1) {
							return A2(
								$elm$parser$Parser$Advanced$Bad,
								true,
								A2($elm$parser$Parser$Advanced$fromState, s, invalid));
						} else {
							var n = _v1.a;
							return A3(
								$elm$parser$Parser$Advanced$Good,
								true,
								toValue(n),
								A2($elm$parser$Parser$Advanced$bumpOffset, floatOffset, s));
						}
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$number = function (c) {
	return function (s) {
		if (A3($elm$parser$Parser$Advanced$isAsciiCode, 48, s.b, s.a)) {
			var zeroOffset = s.b + 1;
			var baseOffset = zeroOffset + 1;
			return A3($elm$parser$Parser$Advanced$isAsciiCode, 120, zeroOffset, s.a) ? A5(
				$elm$parser$Parser$Advanced$finalizeInt,
				c.aZ,
				c.aq,
				baseOffset,
				A2($elm$parser$Parser$Advanced$consumeBase16, baseOffset, s.a),
				s) : (A3($elm$parser$Parser$Advanced$isAsciiCode, 111, zeroOffset, s.a) ? A5(
				$elm$parser$Parser$Advanced$finalizeInt,
				c.aZ,
				c.aw,
				baseOffset,
				A3($elm$parser$Parser$Advanced$consumeBase, 8, baseOffset, s.a),
				s) : (A3($elm$parser$Parser$Advanced$isAsciiCode, 98, zeroOffset, s.a) ? A5(
				$elm$parser$Parser$Advanced$finalizeInt,
				c.aZ,
				c.ai,
				baseOffset,
				A3($elm$parser$Parser$Advanced$consumeBase, 2, baseOffset, s.a),
				s) : A6(
				$elm$parser$Parser$Advanced$finalizeFloat,
				c.aZ,
				c.am,
				c.at,
				c.an,
				_Utils_Tuple2(zeroOffset, 0),
				s)));
		} else {
			return A6(
				$elm$parser$Parser$Advanced$finalizeFloat,
				c.aZ,
				c.am,
				c.at,
				c.an,
				A3($elm$parser$Parser$Advanced$consumeBase, 10, s.b, s.a),
				s);
		}
	};
};
var $elm$parser$Parser$Advanced$int = F2(
	function (expecting, invalid) {
		return $elm$parser$Parser$Advanced$number(
			{
				ai: $elm$core$Result$Err(invalid),
				am: expecting,
				an: $elm$core$Result$Err(invalid),
				aq: $elm$core$Result$Err(invalid),
				at: $elm$core$Result$Ok($elm$core$Basics$identity),
				aZ: invalid,
				aw: $elm$core$Result$Err(invalid)
			});
	});
var $elm$parser$Parser$int = A2($elm$parser$Parser$Advanced$int, $elm$parser$Parser$ExpectingInt, $elm$parser$Parser$ExpectingInt);
var $elm$parser$Parser$ExpectingKeyword = function (a) {
	return {$: 9, a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$core$Basics$not = _Basics_not;
var $elm$parser$Parser$Advanced$keyword = function (_v0) {
	var kwd = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(kwd);
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$isSubString, kwd, s.b, s.ae, s._, s.a);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return (_Utils_eq(newOffset, -1) || (0 <= A3(
			$elm$parser$Parser$Advanced$isSubChar,
			function (c) {
				return $elm$core$Char$isAlphaNum(c) || (c === '_');
			},
			newOffset,
			s.a))) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			$elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{_: newCol, c: s.c, d: s.d, b: newOffset, ae: newRow, a: s.a});
	};
};
var $elm$parser$Parser$keyword = function (kwd) {
	return $elm$parser$Parser$Advanced$keyword(
		A2(
			$elm$parser$Parser$Advanced$Token,
			kwd,
			$elm$parser$Parser$ExpectingKeyword(kwd)));
};
var $elm$parser$Parser$Advanced$lazy = function (thunk) {
	return function (s) {
		var _v0 = thunk(0);
		var parse = _v0;
		return parse(s);
	};
};
var $elm$parser$Parser$lazy = $elm$parser$Parser$Advanced$lazy;
var $author$project$AST$TupleP = function (a) {
	return {$: 5, a: a};
};
var $author$project$HsParser$makeTupleP = function (l) {
	if (l.b && (!l.b.b)) {
		var x = l.a;
		return x;
	} else {
		return $author$project$AST$TupleP(l);
	}
};
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (!_v1.$) {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
	};
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var parse = _v0;
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p1 = _v1.a;
				var step = _v1.b;
				var s1 = _v1.c;
				if (!step.$) {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3($elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var $elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return function (s) {
			return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
		};
	});
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					func(a),
					s1);
			} else {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			}
		};
	});
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$Advanced$revAlways = F2(
	function (_v0, b) {
		return b;
	});
var $elm$parser$Parser$Advanced$skip = F2(
	function (iParser, kParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$parser$Parser$Advanced$revAlways, iParser, kParser);
	});
var $elm$parser$Parser$Advanced$sequenceEndForbidden = F5(
	function (ender, ws, parseItem, sep, revItems) {
		var chompRest = function (item) {
			return A5(
				$elm$parser$Parser$Advanced$sequenceEndForbidden,
				ender,
				ws,
				parseItem,
				sep,
				A2($elm$core$List$cons, item, revItems));
		};
		return A2(
			$elm$parser$Parser$Advanced$skip,
			ws,
			$elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$Advanced$skip,
						sep,
						A2(
							$elm$parser$Parser$Advanced$skip,
							ws,
							A2(
								$elm$parser$Parser$Advanced$map,
								function (item) {
									return $elm$parser$Parser$Advanced$Loop(
										A2($elm$core$List$cons, item, revItems));
								},
								parseItem))),
						A2(
						$elm$parser$Parser$Advanced$map,
						function (_v0) {
							return $elm$parser$Parser$Advanced$Done(
								$elm$core$List$reverse(revItems));
						},
						ender)
					])));
	});
var $elm$parser$Parser$Advanced$sequenceEndMandatory = F4(
	function (ws, parseItem, sep, revItems) {
		return $elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$Advanced$map,
					function (item) {
						return $elm$parser$Parser$Advanced$Loop(
							A2($elm$core$List$cons, item, revItems));
					},
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						parseItem,
						A2(
							$elm$parser$Parser$Advanced$ignorer,
							ws,
							A2($elm$parser$Parser$Advanced$ignorer, sep, ws)))),
					A2(
					$elm$parser$Parser$Advanced$map,
					function (_v0) {
						return $elm$parser$Parser$Advanced$Done(
							$elm$core$List$reverse(revItems));
					},
					$elm$parser$Parser$Advanced$succeed(0))
				]));
	});
var $elm$parser$Parser$Advanced$sequenceEndOptional = F5(
	function (ender, ws, parseItem, sep, revItems) {
		var parseEnd = A2(
			$elm$parser$Parser$Advanced$map,
			function (_v0) {
				return $elm$parser$Parser$Advanced$Done(
					$elm$core$List$reverse(revItems));
			},
			ender);
		return A2(
			$elm$parser$Parser$Advanced$skip,
			ws,
			$elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$Advanced$skip,
						sep,
						A2(
							$elm$parser$Parser$Advanced$skip,
							ws,
							$elm$parser$Parser$Advanced$oneOf(
								_List_fromArray(
									[
										A2(
										$elm$parser$Parser$Advanced$map,
										function (item) {
											return $elm$parser$Parser$Advanced$Loop(
												A2($elm$core$List$cons, item, revItems));
										},
										parseItem),
										parseEnd
									])))),
						parseEnd
					])));
	});
var $elm$parser$Parser$Advanced$sequenceEnd = F5(
	function (ender, ws, parseItem, sep, trailing) {
		var chompRest = function (item) {
			switch (trailing) {
				case 0:
					return A2(
						$elm$parser$Parser$Advanced$loop,
						_List_fromArray(
							[item]),
						A4($elm$parser$Parser$Advanced$sequenceEndForbidden, ender, ws, parseItem, sep));
				case 1:
					return A2(
						$elm$parser$Parser$Advanced$loop,
						_List_fromArray(
							[item]),
						A4($elm$parser$Parser$Advanced$sequenceEndOptional, ender, ws, parseItem, sep));
				default:
					return A2(
						$elm$parser$Parser$Advanced$ignorer,
						A2(
							$elm$parser$Parser$Advanced$skip,
							ws,
							A2(
								$elm$parser$Parser$Advanced$skip,
								sep,
								A2(
									$elm$parser$Parser$Advanced$skip,
									ws,
									A2(
										$elm$parser$Parser$Advanced$loop,
										_List_fromArray(
											[item]),
										A3($elm$parser$Parser$Advanced$sequenceEndMandatory, ws, parseItem, sep))))),
						ender);
			}
		};
		return $elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2($elm$parser$Parser$Advanced$andThen, chompRest, parseItem),
					A2(
					$elm$parser$Parser$Advanced$map,
					function (_v0) {
						return _List_Nil;
					},
					ender)
				]));
	});
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.b, s.ae, s._, s.a);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			$elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{_: newCol, c: s.c, d: s.d, b: newOffset, ae: newRow, a: s.a});
	};
};
var $elm$parser$Parser$Advanced$sequence = function (i) {
	return A2(
		$elm$parser$Parser$Advanced$skip,
		$elm$parser$Parser$Advanced$token(i.t),
		A2(
			$elm$parser$Parser$Advanced$skip,
			i.y,
			A5(
				$elm$parser$Parser$Advanced$sequenceEnd,
				$elm$parser$Parser$Advanced$token(i.u),
				i.y,
				i.v,
				$elm$parser$Parser$Advanced$token(i.x),
				i.z)));
};
var $elm$parser$Parser$Advanced$Forbidden = 0;
var $elm$parser$Parser$Advanced$Mandatory = 2;
var $elm$parser$Parser$Advanced$Optional = 1;
var $elm$parser$Parser$toAdvancedTrailing = function (trailing) {
	switch (trailing) {
		case 0:
			return 0;
		case 1:
			return 1;
		default:
			return 2;
	}
};
var $elm$parser$Parser$Expecting = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$toToken = function (str) {
	return A2(
		$elm$parser$Parser$Advanced$Token,
		str,
		$elm$parser$Parser$Expecting(str));
};
var $elm$parser$Parser$sequence = function (i) {
	return $elm$parser$Parser$Advanced$sequence(
		{
			u: $elm$parser$Parser$toToken(i.u),
			v: i.v,
			x: $elm$parser$Parser$toToken(i.x),
			y: i.y,
			t: $elm$parser$Parser$toToken(i.t),
			z: $elm$parser$Parser$toAdvancedTrailing(i.z)
		});
};
var $author$project$HsParser$spaces = $elm$parser$Parser$chompWhile(
	function (c) {
		return c === ' ';
	});
function $author$project$HsParser$cyclic$pattern() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed($author$project$AST$VarP),
				$author$project$HsParser$identifier),
				A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed(
					$author$project$AST$BooleanP(true)),
				$elm$parser$Parser$keyword('True')),
				A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed(
					$author$project$AST$BooleanP(false)),
				$elm$parser$Parser$keyword('False')),
				A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed($author$project$AST$NumberP),
				$elm$parser$Parser$backtrackable($elm$parser$Parser$int)),
				A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed($author$project$AST$ListP),
				$elm$parser$Parser$sequence(
					{
						u: ']',
						v: $elm$parser$Parser$lazy(
							function (_v0) {
								return $author$project$HsParser$cyclic$pattern();
							}),
						x: ',',
						y: $author$project$HsParser$spaces,
						t: '[',
						z: 0
					})),
				$elm$parser$Parser$backtrackable(
				A2(
					$elm$parser$Parser$keeper,
					$elm$parser$Parser$succeed($author$project$HsParser$makeTupleP),
					$elm$parser$Parser$sequence(
						{
							u: ')',
							v: $elm$parser$Parser$lazy(
								function (_v1) {
									return $author$project$HsParser$cyclic$pattern();
								}),
							x: ',',
							y: $author$project$HsParser$spaces,
							t: '(',
							z: 0
						}))),
				A2(
				$elm$parser$Parser$andThen,
				function (l) {
					var _v3 = $elm$core$List$reverse(l);
					if (!_v3.b) {
						return $elm$parser$Parser$problem('pattern');
					} else {
						var p = _v3.a;
						var ps = _v3.b;
						return $elm$parser$Parser$succeed(
							A3(
								$elm$core$List$foldr,
								$author$project$AST$ConsP,
								p,
								$elm$core$List$reverse(ps)));
					}
				},
				$elm$parser$Parser$sequence(
					{
						u: ')',
						v: $elm$parser$Parser$lazy(
							function (_v2) {
								return $author$project$HsParser$cyclic$pattern();
							}),
						x: ':',
						y: $author$project$HsParser$spaces,
						t: '(',
						z: 0
					}))
			]));
}
var $author$project$HsParser$pattern = $author$project$HsParser$cyclic$pattern();
$author$project$HsParser$cyclic$pattern = function () {
	return $author$project$HsParser$pattern;
};
var $author$project$AST$Boolean = function (a) {
	return {$: 4, a: a};
};
var $author$project$AST$Number = function (a) {
	return {$: 3, a: a};
};
var $author$project$AST$Var = function (a) {
	return {$: 2, a: a};
};
var $author$project$HsParser$identifierList = $elm$parser$Parser$sequence(
	{u: '', v: $author$project$HsParser$identifier, x: '', y: $author$project$HsParser$spaces, t: '', z: 0});
var $author$project$HsParser$infixLeftCont = F3(
	function (operand, table, accum) {
		return $elm$parser$Parser$oneOf(
			_Utils_ap(
				A2(
					$elm$core$List$map,
					function (_v0) {
						var op = _v0.a;
						var func = _v0.b;
						return A2(
							$elm$parser$Parser$andThen,
							A2($author$project$HsParser$infixLeftCont, operand, table),
							A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$ignorer,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed(
											func(accum)),
										$author$project$HsParser$operator(op)),
									$author$project$HsParser$spaces),
								A2($elm$parser$Parser$ignorer, operand, $author$project$HsParser$spaces)));
					},
					table),
				_List_fromArray(
					[
						$elm$parser$Parser$succeed(accum)
					])));
	});
var $author$project$HsParser$infixLeft = F2(
	function (operand, table) {
		return A2(
			$elm$parser$Parser$andThen,
			A2($author$project$HsParser$infixLeftCont, operand, table),
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed($elm$core$Basics$identity),
				A2($elm$parser$Parser$ignorer, operand, $author$project$HsParser$spaces)));
	});
var $author$project$HsParser$infixRightCont = F3(
	function (operand, table, x) {
		return $elm$parser$Parser$oneOf(
			_Utils_ap(
				A2(
					$elm$core$List$map,
					function (_v0) {
						var op = _v0.a;
						var func = _v0.b;
						return A2(
							$elm$parser$Parser$andThen,
							function (y) {
								return A2(
									$elm$parser$Parser$andThen,
									function (r) {
										return $elm$parser$Parser$succeed(
											A2(func, x, r));
									},
									A3($author$project$HsParser$infixRightCont, operand, table, y));
							},
							A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$ignorer,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed($elm$core$Basics$identity),
										$author$project$HsParser$operator(op)),
									$author$project$HsParser$spaces),
								A2($elm$parser$Parser$ignorer, operand, $author$project$HsParser$spaces)));
					},
					table),
				_List_fromArray(
					[
						$elm$parser$Parser$succeed(x)
					])));
	});
var $author$project$HsParser$infixRight = F2(
	function (operand, table) {
		return A2(
			$elm$parser$Parser$andThen,
			A2($author$project$HsParser$infixRightCont, operand, table),
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed($elm$core$Basics$identity),
				A2($elm$parser$Parser$ignorer, operand, $author$project$HsParser$spaces)));
	});
var $author$project$HsParser$makeTuple = function (l) {
	if (l.b && (!l.b.b)) {
		var x = l.a;
		return x;
	} else {
		return $author$project$AST$TupleLit(l);
	}
};
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 8, a: a};
};
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $elm$parser$Parser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
function $author$project$HsParser$cyclic$topExpr() {
	return $author$project$HsParser$cyclic$infix2();
}
function $author$project$HsParser$cyclic$infix2() {
	return A2(
		$author$project$HsParser$infixRight,
		$author$project$HsParser$cyclic$infix3(),
		_List_fromArray(
			[
				_Utils_Tuple2(
				'||',
				$author$project$AST$InfixOp('||'))
			]));
}
function $author$project$HsParser$cyclic$infix3() {
	return A2(
		$author$project$HsParser$infixRight,
		$author$project$HsParser$cyclic$infix4(),
		_List_fromArray(
			[
				_Utils_Tuple2(
				'&&',
				$author$project$AST$InfixOp('&&'))
			]));
}
function $author$project$HsParser$cyclic$infix4() {
	return A2(
		$author$project$HsParser$infixLeft,
		$author$project$HsParser$cyclic$infix5(),
		_List_fromArray(
			[
				_Utils_Tuple2(
				'==',
				$author$project$AST$InfixOp('==')),
				_Utils_Tuple2(
				'/=',
				$author$project$AST$InfixOp('/=')),
				_Utils_Tuple2(
				'<=',
				$author$project$AST$InfixOp('<=')),
				_Utils_Tuple2(
				'>=',
				$author$project$AST$InfixOp('>=')),
				_Utils_Tuple2(
				'<',
				$author$project$AST$InfixOp('<')),
				_Utils_Tuple2(
				'>',
				$author$project$AST$InfixOp('>'))
			]));
}
function $author$project$HsParser$cyclic$infix5() {
	return A2(
		$author$project$HsParser$infixRight,
		$author$project$HsParser$cyclic$infix6(),
		_List_fromArray(
			[
				_Utils_Tuple2(':', $author$project$AST$Cons),
				_Utils_Tuple2(
				'++',
				$author$project$AST$InfixOp('++'))
			]));
}
function $author$project$HsParser$cyclic$infix6() {
	return A2(
		$author$project$HsParser$infixLeft,
		$author$project$HsParser$cyclic$infix7(),
		_List_fromArray(
			[
				_Utils_Tuple2(
				'+',
				$author$project$AST$InfixOp('+')),
				_Utils_Tuple2(
				'-',
				$author$project$AST$InfixOp('-'))
			]));
}
function $author$project$HsParser$cyclic$infix7() {
	return A2(
		$author$project$HsParser$infixLeft,
		$author$project$HsParser$cyclic$applicativeExpr(),
		_List_fromArray(
			[
				_Utils_Tuple2(
				'*',
				$author$project$AST$InfixOp('*'))
			]));
}
function $author$project$HsParser$cyclic$applicativeExpr() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$author$project$HsParser$cyclic$if_then_else(),
				$author$project$HsParser$cyclic$lambda(),
				$author$project$HsParser$cyclic$application()
			]));
}
function $author$project$HsParser$cyclic$application() {
	return A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed(
				F2(
					function (e0, args) {
						if (!args.b) {
							return e0;
						} else {
							return A2($author$project$AST$App, e0, args);
						}
					})),
			A2(
				$elm$parser$Parser$ignorer,
				$author$project$HsParser$cyclic$delimited(),
				$author$project$HsParser$spaces)),
		$author$project$HsParser$cyclic$delimitedList());
}
function $author$project$HsParser$cyclic$delimitedList() {
	return $elm$parser$Parser$sequence(
		{
			u: '',
			v: $author$project$HsParser$cyclic$delimited(),
			x: '',
			y: $author$project$HsParser$spaces,
			t: '',
			z: 0
		});
}
function $author$project$HsParser$cyclic$delimited() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed($author$project$AST$Var),
				$author$project$HsParser$identifier),
				A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed($author$project$AST$Number),
				$elm$parser$Parser$backtrackable($elm$parser$Parser$int)),
				A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed(
					$author$project$AST$Boolean(true)),
				$elm$parser$Parser$keyword('True')),
				A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed(
					$author$project$AST$Boolean(false)),
				$elm$parser$Parser$keyword('False')),
				$elm$parser$Parser$backtrackable(
				A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed($author$project$AST$Var),
						$elm$parser$Parser$symbol('(')),
					A2(
						$elm$parser$Parser$ignorer,
						$author$project$HsParser$infixOperator,
						$elm$parser$Parser$symbol(')')))),
				$author$project$HsParser$cyclic$literalTuple(),
				$author$project$HsParser$cyclic$literalList()
			]));
}
function $author$project$HsParser$cyclic$if_then_else() {
	return A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed($author$project$AST$IfThenElse),
						$elm$parser$Parser$keyword('if')),
					$author$project$HsParser$spaces),
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$lazy(
								function (_v3) {
									return $author$project$HsParser$cyclic$topExpr();
								}),
							$author$project$HsParser$spaces),
						$elm$parser$Parser$keyword('then')),
					$author$project$HsParser$spaces)),
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$lazy(
							function (_v4) {
								return $author$project$HsParser$cyclic$topExpr();
							}),
						$author$project$HsParser$spaces),
					$elm$parser$Parser$keyword('else')),
				$author$project$HsParser$spaces)),
		$elm$parser$Parser$lazy(
			function (_v5) {
				return $author$project$HsParser$cyclic$topExpr();
			}));
}
function $author$project$HsParser$cyclic$lambda() {
	return A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed($author$project$AST$Lam),
					$elm$parser$Parser$symbol('\\')),
				$author$project$HsParser$spaces),
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					A2($elm$parser$Parser$ignorer, $author$project$HsParser$identifierList, $author$project$HsParser$spaces),
					$author$project$HsParser$operator('->')),
				$author$project$HsParser$spaces)),
		$elm$parser$Parser$lazy(
			function (_v2) {
				return $author$project$HsParser$cyclic$topExpr();
			}));
}
function $author$project$HsParser$cyclic$literalList() {
	return A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($author$project$AST$ListLit),
		$elm$parser$Parser$sequence(
			{
				u: ']',
				v: $elm$parser$Parser$lazy(
					function (_v1) {
						return $author$project$HsParser$cyclic$topExpr();
					}),
				x: ',',
				y: $author$project$HsParser$spaces,
				t: '[',
				z: 0
			}));
}
function $author$project$HsParser$cyclic$literalTuple() {
	return A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($author$project$HsParser$makeTuple),
		$elm$parser$Parser$sequence(
			{
				u: ')',
				v: $elm$parser$Parser$lazy(
					function (_v0) {
						return $author$project$HsParser$cyclic$topExpr();
					}),
				x: ',',
				y: $author$project$HsParser$spaces,
				t: '(',
				z: 0
			}));
}
var $author$project$HsParser$topExpr = $author$project$HsParser$cyclic$topExpr();
$author$project$HsParser$cyclic$topExpr = function () {
	return $author$project$HsParser$topExpr;
};
var $author$project$HsParser$infix2 = $author$project$HsParser$cyclic$infix2();
$author$project$HsParser$cyclic$infix2 = function () {
	return $author$project$HsParser$infix2;
};
var $author$project$HsParser$infix3 = $author$project$HsParser$cyclic$infix3();
$author$project$HsParser$cyclic$infix3 = function () {
	return $author$project$HsParser$infix3;
};
var $author$project$HsParser$infix4 = $author$project$HsParser$cyclic$infix4();
$author$project$HsParser$cyclic$infix4 = function () {
	return $author$project$HsParser$infix4;
};
var $author$project$HsParser$infix5 = $author$project$HsParser$cyclic$infix5();
$author$project$HsParser$cyclic$infix5 = function () {
	return $author$project$HsParser$infix5;
};
var $author$project$HsParser$infix6 = $author$project$HsParser$cyclic$infix6();
$author$project$HsParser$cyclic$infix6 = function () {
	return $author$project$HsParser$infix6;
};
var $author$project$HsParser$infix7 = $author$project$HsParser$cyclic$infix7();
$author$project$HsParser$cyclic$infix7 = function () {
	return $author$project$HsParser$infix7;
};
var $author$project$HsParser$applicativeExpr = $author$project$HsParser$cyclic$applicativeExpr();
$author$project$HsParser$cyclic$applicativeExpr = function () {
	return $author$project$HsParser$applicativeExpr;
};
var $author$project$HsParser$application = $author$project$HsParser$cyclic$application();
$author$project$HsParser$cyclic$application = function () {
	return $author$project$HsParser$application;
};
var $author$project$HsParser$delimitedList = $author$project$HsParser$cyclic$delimitedList();
$author$project$HsParser$cyclic$delimitedList = function () {
	return $author$project$HsParser$delimitedList;
};
var $author$project$HsParser$delimited = $author$project$HsParser$cyclic$delimited();
$author$project$HsParser$cyclic$delimited = function () {
	return $author$project$HsParser$delimited;
};
var $author$project$HsParser$if_then_else = $author$project$HsParser$cyclic$if_then_else();
$author$project$HsParser$cyclic$if_then_else = function () {
	return $author$project$HsParser$if_then_else;
};
var $author$project$HsParser$lambda = $author$project$HsParser$cyclic$lambda();
$author$project$HsParser$cyclic$lambda = function () {
	return $author$project$HsParser$lambda;
};
var $author$project$HsParser$literalList = $author$project$HsParser$cyclic$literalList();
$author$project$HsParser$cyclic$literalList = function () {
	return $author$project$HsParser$literalList;
};
var $author$project$HsParser$literalTuple = $author$project$HsParser$cyclic$literalTuple();
$author$project$HsParser$cyclic$literalTuple = function () {
	return $author$project$HsParser$literalTuple;
};
var $author$project$HsParser$infixEquation = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					F4(
						function (p1, fun, p2, e) {
							return A3(
								$author$project$AST$Equation,
								fun,
								_List_fromArray(
									[p1, p2]),
								e);
						})),
				A2($elm$parser$Parser$ignorer, $author$project$HsParser$pattern, $author$project$HsParser$spaces)),
			A2($elm$parser$Parser$ignorer, $author$project$HsParser$infixOperator, $author$project$HsParser$spaces)),
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				A2($elm$parser$Parser$ignorer, $author$project$HsParser$pattern, $author$project$HsParser$spaces),
				$author$project$HsParser$operator('=')),
			$author$project$HsParser$spaces)),
	$author$project$HsParser$topExpr);
var $author$project$HsParser$patternList = $elm$parser$Parser$sequence(
	{u: '', v: $author$project$HsParser$pattern, x: '', y: $author$project$HsParser$spaces, t: '', z: 0});
var $author$project$HsParser$prefixEquation = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			$elm$parser$Parser$succeed($author$project$AST$Equation),
			$author$project$HsParser$identifier),
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				A2($elm$parser$Parser$ignorer, $author$project$HsParser$patternList, $author$project$HsParser$spaces),
				$author$project$HsParser$operator('=')),
			$author$project$HsParser$spaces)),
	$author$project$HsParser$topExpr);
var $author$project$AST$TypeSig = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$chompUntilEndOr = function (str) {
	return function (s) {
		var _v0 = A5(_Parser_findSubString, str, s.b, s.ae, s._, s.a);
		var newOffset = _v0.a;
		var newRow = _v0.b;
		var newCol = _v0.c;
		var adjustedOffset = (newOffset < 0) ? $elm$core$String$length(s.a) : newOffset;
		return A3(
			$elm$parser$Parser$Advanced$Good,
			_Utils_cmp(s.b, adjustedOffset) < 0,
			0,
			{_: newCol, c: s.c, d: s.d, b: adjustedOffset, ae: newRow, a: s.a});
	};
};
var $elm$parser$Parser$chompUntilEndOr = $elm$parser$Parser$Advanced$chompUntilEndOr;
var $author$project$HsParser$identifierOrOperator = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			$author$project$HsParser$identifier,
			A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$ignorer,
				$elm$parser$Parser$succeed($elm$core$Basics$identity),
				$elm$parser$Parser$symbol('(')),
			A2(
				$elm$parser$Parser$ignorer,
				$author$project$HsParser$infixOperator,
				$elm$parser$Parser$symbol(')')))
		]));
var $author$project$HsParser$typeSignature = A2(
	$elm$parser$Parser$keeper,
	A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($author$project$AST$TypeSig),
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				A2($elm$parser$Parser$ignorer, $author$project$HsParser$identifierOrOperator, $author$project$HsParser$spaces),
				$author$project$HsParser$operator('::')),
			$author$project$HsParser$spaces)),
	$elm$parser$Parser$getChompedString(
		$elm$parser$Parser$chompUntilEndOr('\n')));
var $author$project$HsParser$declaration = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			$elm$parser$Parser$backtrackable($author$project$HsParser$typeSignature),
			$elm$parser$Parser$backtrackable($author$project$HsParser$infixEquation),
			$author$project$HsParser$prefixEquation
		]));
var $author$project$HsParser$whitespace = $elm$parser$Parser$chompWhile(
	function (c) {
		return (c === ' ') || ((c === '\n') || (c === '\r'));
	});
var $author$project$HsParser$declList = $elm$parser$Parser$sequence(
	{u: '', v: $author$project$HsParser$declaration, x: '', y: $author$project$HsParser$whitespace, t: '', z: 2});
var $elm$parser$Parser$ExpectingEnd = {$: 10};
var $elm$parser$Parser$Advanced$end = function (x) {
	return function (s) {
		return _Utils_eq(
			$elm$core$String$length(s.a),
			s.b) ? A3($elm$parser$Parser$Advanced$Good, false, 0, s) : A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var $elm$parser$Parser$end = $elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd);
var $author$project$HsParser$declListEnd = A2(
	$elm$parser$Parser$keeper,
	$elm$parser$Parser$succeed($elm$core$Basics$identity),
	A2($elm$parser$Parser$ignorer, $author$project$HsParser$declList, $elm$parser$Parser$end));
var $author$project$Prelude$prelude = '\n(&&) :: Bool -> Bool -> Bool\nFalse && x = False\nTrue  && x = x\n\n(||) :: Bool -> Bool -> Bool\nFalse || x = x\nTrue  || x = True\n\nnot :: Bool -> Bool\nnot True = False\nnot False = True\n\neven :: Int -> Bool\neven x = mod x 2 == 0\n\nodd :: Int -> Bool\nodd x = mod x 2 == 1\n\nmin :: Int -> Int -> Int\nmin x y = if x<=y then x else y\n\nmax :: Int -> Int -> Int\nmax x y = if x<=y then y else x\n\nfst :: (a,b) -> a\nfst (x,y) = x\n\nsnd :: (a,b) -> b\nsnd (x,y) = y\n\nhead :: [a] -> a\nhead (x:xs) = x\n\ntail :: [a] -> [a]\ntail (x:xs) = xs\n\nlength :: [a] -> Int\nlength [] = 0\nlength (x:xs) = 1 + length xs\n\n(++) :: [a] -> [a] -> [a]\n[] ++ ys = ys\n(x:xs) ++ ys = x:(xs++ys)\n\nreverse :: [a] -> [a]\nreverse [] = []\nreverse (x:xs) = reverse xs ++ [x]\n\nsum :: [Int] -> Int\nsum [] = 0\nsum (x:xs) = x + sum xs\n\nproduct :: [Int] -> Int\nproduct [] = 1\nproduct (x:xs) = x * product xs\n\ntake :: Int -> [a] -> [a]\ntake 0 xs = []\ntake n [] = []\ntake n (x:xs) = x : take (n-1) xs\n\ndrop :: Int -> [a] -> [a]\ndrop 0 xs = xs\ndrop n [] = []\ndrop n (x:xs) = drop (n-1) xs\n\nany :: (a -> Bool) -> [a] -> Bool\nany f [] = False\nany f (x:xs) = f x || any f xs\n\nall :: (a -> Bool) -> [a] -> Bool\nall f [] = True\nall f (x:xs) = f x && all f xs\n\nmap :: (a->b) -> [a] -> [b]\nmap f [] = []\nmap f (x:xs) = f x : map f xs\n\nfilter :: (a -> Bool) -> [a] -> [a]\nfilter f [] = []\nfilter f (x:xs)= if f x then x : filter f xs else filter f xs\n\nzip :: [a] -> [b] -> [(a,b)]\nzip [] ys = []\nzip xs [] = []\nzip (x:xs) (y:ys) = (x,y) : zip xs ys\n\nzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]\nzipWith f [] ys = []\nzipWith f xs [] = []\nzipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys\n\nfoldr :: (a -> b -> b) -> b -> [a] -> b\nfoldr f z [] = z\nfoldr f z (x:xs) = f x (foldr f z xs)\n\nfoldl :: (a -> b -> a) -> a -> [b] -> a\nfoldl f z [] = z\nfoldl f z (x:xs) = foldl f (f z x) xs\n\ntakeWhile :: (a -> Bool) -> [a] -> [a]\ntakeWhile p [] = []\ntakeWhile p (x:xs) = if p x then x : takeWhile p xs else []\n\ndropWhile :: (a -> Bool) -> [a] -> [a]\ndropWhile p [] = []\ndropWhile p (x:xs) = if p x then dropWhile p xs else x:xs\n';
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {_: col, a2: problem, ae: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.ae, p._, p.a2);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 0:
					return list;
				case 1:
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0;
		var _v1 = parse(
			{_: 1, c: _List_Nil, d: 1, b: 0, ae: 1, a: src});
		if (!_v1.$) {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (!_v0.$) {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $author$project$Prelude$declarations = function () {
	var _v0 = A2($elm$parser$Parser$run, $author$project$HsParser$declList, $author$project$Prelude$prelude);
	if (!_v0.$) {
		var l = _v0.a;
		return l;
	} else {
		return _List_Nil;
	}
}();
var $author$project$Eval$isWeakNormalForm = function (expr) {
	switch (expr.$) {
		case 0:
			return false;
		case 1:
			return true;
		case 2:
			return true;
		case 3:
			return true;
		case 4:
			return true;
		case 5:
			return true;
		case 6:
			return true;
		case 7:
			return true;
		case 8:
			return false;
		case 9:
			return false;
		default:
			return false;
	}
};
var $author$project$Eval$arithOp = F3(
	function (op, func, args) {
		if ((args.b && args.b.b) && (!args.b.b.b)) {
			if ((args.a.$ === 3) && (args.b.a.$ === 3)) {
				var x = args.a.a;
				var _v1 = args.b;
				var y = _v1.a.a;
				return $elm$core$Maybe$Just(
					_Utils_Tuple2(
						$author$project$AST$Number(
							A2(func, x, y)),
						op));
			} else {
				var arg1 = args.a;
				var _v2 = args.b;
				var arg2 = _v2.a;
				return ($author$project$Eval$isWeakNormalForm(arg1) && $author$project$Eval$isWeakNormalForm(arg2)) ? $elm$core$Maybe$Just(
					_Utils_Tuple2(
						$author$project$AST$Fail('type error: operator requires numbers'),
						op)) : $elm$core$Maybe$Nothing;
			}
		} else {
			return ($elm$core$List$length(args) > 2) ? $elm$core$Maybe$Just(
				_Utils_Tuple2(
					$author$project$AST$Fail('type error: wrong number of arguments'),
					op)) : $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Eval$compareOp = F3(
	function (op, func, args) {
		if ((((args.b && (args.a.$ === 3)) && args.b.b) && (args.b.a.$ === 3)) && (!args.b.b.b)) {
			var x = args.a.a;
			var _v1 = args.b;
			var y = _v1.a.a;
			return $elm$core$Maybe$Just(
				_Utils_Tuple2(
					$author$project$AST$Boolean(
						A2(func, x, y)),
					op));
		} else {
			return ($elm$core$List$length(args) > 2) ? $elm$core$Maybe$Just(
				_Utils_Tuple2(
					$author$project$AST$Fail('type error: wrong number of arguments'),
					op)) : $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $author$project$Eval$isNormalForm = function (expr) {
	switch (expr.$) {
		case 0:
			return false;
		case 1:
			return true;
		case 2:
			return true;
		case 3:
			return true;
		case 4:
			return true;
		case 5:
			var e1 = expr.a;
			var e2 = expr.b;
			return $author$project$Eval$isNormalForm(e1) && $author$project$Eval$isNormalForm(e2);
		case 6:
			var items = expr.a;
			return A2($elm$core$List$all, $author$project$Eval$isNormalForm, items);
		case 7:
			var items = expr.a;
			return A2($elm$core$List$all, $author$project$Eval$isNormalForm, items);
		case 8:
			return false;
		case 9:
			return false;
		default:
			return false;
	}
};
var $author$project$Eval$comparePoly = F3(
	function (op, func, args) {
		if ((args.b && args.b.b) && (!args.b.b.b)) {
			var arg1 = args.a;
			var _v1 = args.b;
			var arg2 = _v1.a;
			return ($author$project$Eval$isNormalForm(arg1) && $author$project$Eval$isNormalForm(arg2)) ? $elm$core$Maybe$Just(
				_Utils_Tuple2(
					$author$project$AST$Boolean(
						A2(func, arg1, arg2)),
					op)) : $elm$core$Maybe$Nothing;
		} else {
			return ($elm$core$List$length(args) > 2) ? $elm$core$Maybe$Just(
				_Utils_Tuple2(
					$author$project$AST$Fail('type error: wrong number of arguments'),
					op)) : $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Eval$equalExpr = F2(
	function (expr1, expr2) {
		var _v0 = _Utils_Tuple2(expr1, expr2);
		_v0$11:
		while (true) {
			switch (_v0.a.$) {
				case 0:
					if (!_v0.b.$) {
						var _v1 = _v0.a;
						var e1 = _v1.a;
						var args1 = _v1.b;
						var _v2 = _v0.b;
						var e2 = _v2.a;
						var args2 = _v2.b;
						return A2($author$project$Eval$equalExpr, e1, e2) && A2($author$project$Eval$equalList, args1, args2);
					} else {
						break _v0$11;
					}
				case 1:
					if (_v0.b.$ === 1) {
						var _v3 = _v0.a;
						var xs1 = _v3.a;
						var e1 = _v3.b;
						var _v4 = _v0.b;
						var xs2 = _v4.a;
						var e2 = _v4.b;
						return _Utils_eq(xs1, xs2) && A2($author$project$Eval$equalExpr, e1, e2);
					} else {
						break _v0$11;
					}
				case 2:
					if (_v0.b.$ === 2) {
						var x1 = _v0.a.a;
						var x2 = _v0.b.a;
						return _Utils_eq(x1, x2);
					} else {
						break _v0$11;
					}
				case 3:
					if (_v0.b.$ === 3) {
						var n1 = _v0.a.a;
						var n2 = _v0.b.a;
						return _Utils_eq(n1, n2);
					} else {
						break _v0$11;
					}
				case 4:
					if (_v0.b.$ === 4) {
						var b1 = _v0.a.a;
						var b2 = _v0.b.a;
						return _Utils_eq(b1, b2);
					} else {
						break _v0$11;
					}
				case 5:
					if (_v0.b.$ === 5) {
						var _v5 = _v0.a;
						var e1 = _v5.a;
						var e2 = _v5.b;
						var _v6 = _v0.b;
						var e3 = _v6.a;
						var e4 = _v6.b;
						return A2($author$project$Eval$equalExpr, e1, e3) && A2($author$project$Eval$equalExpr, e2, e4);
					} else {
						break _v0$11;
					}
				case 6:
					if (_v0.b.$ === 6) {
						var items1 = _v0.a.a;
						var items2 = _v0.b.a;
						return A2($author$project$Eval$equalList, items1, items2);
					} else {
						break _v0$11;
					}
				case 7:
					if (_v0.b.$ === 7) {
						var items1 = _v0.a.a;
						var items2 = _v0.b.a;
						return A2($author$project$Eval$equalList, items1, items2);
					} else {
						break _v0$11;
					}
				case 8:
					if (_v0.b.$ === 8) {
						var _v7 = _v0.a;
						var op1 = _v7.a;
						var e1 = _v7.b;
						var e2 = _v7.c;
						var _v8 = _v0.b;
						var op2 = _v8.a;
						var e3 = _v8.b;
						var e4 = _v8.c;
						return _Utils_eq(op1, op2) && (A2($author$project$Eval$equalExpr, e1, e3) && A2($author$project$Eval$equalExpr, e2, e4));
					} else {
						break _v0$11;
					}
				case 9:
					if (_v0.b.$ === 9) {
						var _v9 = _v0.a;
						var e1 = _v9.a;
						var e2 = _v9.b;
						var e3 = _v9.c;
						var _v10 = _v0.b;
						var e4 = _v10.a;
						var e5 = _v10.b;
						var e6 = _v10.c;
						return A2($author$project$Eval$equalExpr, e1, e4) && (A2($author$project$Eval$equalExpr, e2, e5) && A2($author$project$Eval$equalExpr, e3, e6));
					} else {
						break _v0$11;
					}
				default:
					if (_v0.b.$ === 10) {
						var msg1 = _v0.a.a;
						var msg2 = _v0.b.a;
						return _Utils_eq(msg1, msg2);
					} else {
						break _v0$11;
					}
			}
		}
		return false;
	});
var $author$project$Eval$equalList = F2(
	function (items1, items2) {
		return A2(
			$elm$core$List$all,
			$elm$core$Basics$identity,
			A3($elm$core$List$map2, $author$project$Eval$equalExpr, items1, items2));
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Basics$modBy = _Basics_modBy;
var $author$project$Eval$primitives = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2(
			'+',
			A2($author$project$Eval$arithOp, '+', $elm$core$Basics$add)),
			_Utils_Tuple2(
			'-',
			A2($author$project$Eval$arithOp, '-', $elm$core$Basics$sub)),
			_Utils_Tuple2(
			'*',
			A2($author$project$Eval$arithOp, '*', $elm$core$Basics$mul)),
			_Utils_Tuple2(
			'div',
			A2($author$project$Eval$arithOp, 'div', $elm$core$Basics$idiv)),
			_Utils_Tuple2(
			'mod',
			A2(
				$author$project$Eval$arithOp,
				'mod',
				F2(
					function (x, y) {
						return A2($elm$core$Basics$modBy, y, x);
					}))),
			_Utils_Tuple2(
			'==',
			A2($author$project$Eval$comparePoly, '==', $author$project$Eval$equalExpr)),
			_Utils_Tuple2(
			'/=',
			A2(
				$author$project$Eval$comparePoly,
				'/=',
				F2(
					function (x, y) {
						return !A2($author$project$Eval$equalExpr, x, y);
					}))),
			_Utils_Tuple2(
			'>=',
			A2($author$project$Eval$compareOp, '>=', $elm$core$Basics$ge)),
			_Utils_Tuple2(
			'<=',
			A2($author$project$Eval$compareOp, '<=', $elm$core$Basics$le)),
			_Utils_Tuple2(
			'>',
			A2($author$project$Eval$compareOp, '>', $elm$core$Basics$gt)),
			_Utils_Tuple2(
			'<',
			A2($author$project$Eval$compareOp, '<', $elm$core$Basics$lt))
		]));
var $author$project$Prelude$functions = A2($author$project$Eval$collectFunctions, $author$project$Prelude$declarations, $author$project$Eval$primitives);
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$HsParser$topExprEnd = A2(
	$elm$parser$Parser$keeper,
	$elm$parser$Parser$succeed($elm$core$Basics$identity),
	A2($elm$parser$Parser$ignorer, $author$project$HsParser$topExpr, $elm$parser$Parser$end));
var $author$project$Haskelite$init = function (config) {
	var outputExpr = A2(
		$elm$core$Result$mapError,
		$author$project$Pretty$deadEndsToString,
		A2($elm$parser$Parser$run, $author$project$HsParser$topExprEnd, config.g));
	var outputDecls = A2(
		$elm$core$Result$mapError,
		$author$project$Pretty$deadEndsToString,
		A2($elm$parser$Parser$run, $author$project$HsParser$declListEnd, config.Z));
	var _v0 = _Utils_Tuple2(outputExpr, outputDecls);
	if ((!_v0.a.$) && (!_v0.b.$)) {
		var expr = _v0.a.a;
		var decls = _v0.b.a;
		return _Utils_Tuple2(
			{
				g: expr,
				C: A2($author$project$Eval$collectFunctions, decls, $author$project$Prelude$functions),
				S: config.Z,
				T: config.g,
				I: 1,
				N: outputDecls,
				O: outputExpr,
				l: _List_Nil
			},
			$elm$core$Platform$Cmd$none);
	} else {
		return _Utils_Tuple2(
			{
				g: $author$project$AST$Fail('syntax'),
				C: $author$project$Prelude$functions,
				S: config.Z,
				T: config.g,
				I: 0,
				N: outputDecls,
				O: outputExpr,
				l: _List_Nil
			},
			$elm$core$Platform$Cmd$none);
	}
};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Haskelite$subscriptions = function (_v0) {
	return $elm$core$Platform$Sub$none;
};
var $author$project$Haskelite$editUpdate = F2(
	function (msg, model) {
		switch (msg.$) {
			case 4:
				var string = msg.a;
				var output = A2(
					$elm$core$Result$mapError,
					$author$project$Pretty$deadEndsToString,
					A2($elm$parser$Parser$run, $author$project$HsParser$topExprEnd, string));
				return _Utils_update(
					model,
					{T: string, O: output});
			case 5:
				var string = msg.a;
				var output = A2(
					$elm$core$Result$mapError,
					$author$project$Pretty$deadEndsToString,
					A2($elm$parser$Parser$run, $author$project$HsParser$declListEnd, string));
				return _Utils_update(
					model,
					{S: string, N: output});
			case 7:
				var _v1 = _Utils_Tuple2(model.O, model.N);
				if ((!_v1.a.$) && (!_v1.b.$)) {
					var expr = _v1.a.a;
					var decls = _v1.b.a;
					return _Utils_update(
						model,
						{
							g: expr,
							C: A2($author$project$Eval$collectFunctions, decls, $author$project$Prelude$functions),
							I: 1,
							l: _List_Nil
						});
				} else {
					return model;
				}
			default:
				return model;
		}
	});
var $elm_community$list_extra$List$Extra$last = function (items) {
	last:
	while (true) {
		if (!items.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!items.b.b) {
				var x = items.a;
				return $elm$core$Maybe$Just(x);
			} else {
				var rest = items.b;
				var $temp$items = rest;
				items = $temp$items;
				continue last;
			}
		}
	}
};
var $author$project$Eval$unwindArgs = F2(
	function (e, args) {
		unwindArgs:
		while (true) {
			if (!e.$) {
				var e1 = e.a;
				var es = e.b;
				var $temp$e = e1,
					$temp$args = _Utils_ap(es, args);
				e = $temp$e;
				args = $temp$args;
				continue unwindArgs;
			} else {
				return _Utils_Tuple2(e, args);
			}
		}
	});
var $author$project$Eval$redex = F2(
	function (functions, expr) {
		redex:
		while (true) {
			_v0$4:
			while (true) {
				switch (expr.$) {
					case 0:
						var e1 = expr.a;
						var es = expr.b;
						var _v1 = A2($author$project$Eval$unwindArgs, e1, es);
						switch (_v1.a.$) {
							case 1:
								var _v2 = _v1.a;
								var xs = _v2.a;
								var e0 = _v2.b;
								var args = _v1.b;
								var alt = _Utils_Tuple3(
									A2($elm$core$List$map, $author$project$AST$VarP, xs),
									e0,
									'beta-reduction');
								return A2(
									$author$project$Eval$dispatchAlts,
									_List_fromArray(
										[alt]),
									args);
							case 2:
								var fun = _v1.a.a;
								var args = _v1.b;
								var _v3 = A2($elm$core$Dict$get, fun, functions);
								if (!_v3.$) {
									var semantics = _v3.a;
									return semantics(args);
								} else {
									return $elm$core$Maybe$Just(
										_Utils_Tuple2(
											$author$project$AST$Fail('undefined function'),
											fun));
								}
							default:
								return $elm$core$Maybe$Just(
									_Utils_Tuple2(
										$author$project$AST$Fail('invalid function'),
										'error'));
						}
					case 5:
						if (expr.b.$ === 6) {
							var e1 = expr.a;
							var l = expr.b.a;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(
									$author$project$AST$ListLit(
										A2($elm$core$List$cons, e1, l)),
									'constructor'));
						} else {
							break _v0$4;
						}
					case 8:
						var op = expr.a;
						var e1 = expr.b;
						var e2 = expr.c;
						var $temp$functions = functions,
							$temp$expr = A2(
							$author$project$AST$App,
							$author$project$AST$Var(op),
							_List_fromArray(
								[e1, e2]));
						functions = $temp$functions;
						expr = $temp$expr;
						continue redex;
					case 9:
						var e1 = expr.a;
						var e2 = expr.b;
						var e3 = expr.c;
						if (e1.$ === 4) {
							if (e1.a) {
								return $elm$core$Maybe$Just(
									_Utils_Tuple2(e2, 'if-True'));
							} else {
								return $elm$core$Maybe$Just(
									_Utils_Tuple2(e3, 'if-False'));
							}
						} else {
							return $author$project$Eval$isWeakNormalForm(e1) ? $elm$core$Maybe$Just(
								_Utils_Tuple2(
									$author$project$AST$Fail('type error: if requires a boolean'),
									'if')) : $elm$core$Maybe$Nothing;
						}
					default:
						break _v0$4;
				}
			}
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Eval$redexCtx = F3(
	function (functions, expr, ctx) {
		return A2(
			$elm$core$Maybe$andThen,
			function (subexpr) {
				return A2(
					$elm$core$Maybe$andThen,
					function (result) {
						if (result.a.$ === 10) {
							var err = result.a.a;
							var info = result.b;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(
									$author$project$AST$Fail(err),
									info));
						} else {
							var _new = result.a;
							var info = result.b;
							return $elm$core$Maybe$Just(
								_Utils_Tuple2(
									A2(ctx.m, _new, expr),
									info));
						}
					},
					A2($author$project$Eval$redex, functions, subexpr));
			},
			ctx.j(expr));
	});
var $author$project$Context$app0 = {
	j: function (e) {
		if (!e.$) {
			var e0 = e.a;
			return $elm$core$Maybe$Just(e0);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	},
	m: F2(
		function (e0, e) {
			if (!e.$) {
				var args = e.b;
				return A2($author$project$AST$App, e0, args);
			} else {
				return e;
			}
		})
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (!_v0.$) {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (!_v0.$) {
			var subTree = _v0.a;
			var newSub = A4($elm$core$Array$setHelp, shift - $elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _v0.a;
			var newLeaf = A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, values);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var $elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, tail)) : A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4($elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var $arturopala$elm_monocle$Monocle$Common$array = function (index) {
	return {
		j: $elm$core$Array$get(index),
		m: $elm$core$Array$set(index)
	};
};
var $arturopala$elm_monocle$Monocle$Optional$Optional = F2(
	function (getOption, set) {
		return {j: getOption, m: set};
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $arturopala$elm_monocle$Monocle$Optional$flip = F3(
	function (f, b, a) {
		return A2(f, a, b);
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $arturopala$elm_monocle$Monocle$Optional$compose = F2(
	function (outer, inner) {
		var set = F2(
			function (c, a) {
				return A2(
					$elm$core$Maybe$withDefault,
					a,
					A2(
						$elm$core$Maybe$map,
						A2(
							$elm$core$Basics$composeR,
							inner.m(c),
							A2($arturopala$elm_monocle$Monocle$Optional$flip, outer.m, a)),
						outer.j(a)));
			});
		var getOption = function (a) {
			var _v0 = outer.j(a);
			if (!_v0.$) {
				var x = _v0.a;
				return inner.j(x);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		};
		return A2($arturopala$elm_monocle$Monocle$Optional$Optional, getOption, set);
	});
var $arturopala$elm_monocle$Monocle$Lens$Lens = F2(
	function (get, set) {
		return {aa: get, m: set};
	});
var $arturopala$elm_monocle$Monocle$Lens$fromIso = function (iso) {
	var set = F2(
		function (b, _v0) {
			return iso.F(b);
		});
	return A2($arturopala$elm_monocle$Monocle$Lens$Lens, iso.aa, set);
};
var $arturopala$elm_monocle$Monocle$Optional$fromLens = function (lens) {
	var getOption = function (a) {
		return $elm$core$Maybe$Just(
			lens.aa(a));
	};
	return A2($arturopala$elm_monocle$Monocle$Optional$Optional, getOption, lens.m);
};
var $arturopala$elm_monocle$Monocle$Iso$Iso = F2(
	function (get, reverseGet) {
		return {aa: get, F: reverseGet};
	});
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{i: nodeList, e: nodeListSize, h: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $arturopala$elm_monocle$Monocle$Common$listToArray = A2($arturopala$elm_monocle$Monocle$Iso$Iso, $elm$core$Array$fromList, $elm$core$Array$toList);
var $arturopala$elm_monocle$Monocle$Common$list = function (index) {
	return A2(
		$arturopala$elm_monocle$Monocle$Optional$compose,
		$arturopala$elm_monocle$Monocle$Optional$fromLens(
			$arturopala$elm_monocle$Monocle$Lens$fromIso($arturopala$elm_monocle$Monocle$Common$listToArray)),
		$arturopala$elm_monocle$Monocle$Common$array(index));
};
var $author$project$Context$appArg = function (i) {
	return {
		j: function (e) {
			if (!e.$) {
				var args = e.b;
				return A2(
					function ($) {
						return $.j;
					},
					$arturopala$elm_monocle$Monocle$Common$list(i),
					args);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		},
		m: F2(
			function (n, e) {
				if (!e.$) {
					var e0 = e.a;
					var args = e.b;
					return A2(
						$author$project$AST$App,
						e0,
						A3(
							function ($) {
								return $.m;
							},
							$arturopala$elm_monocle$Monocle$Common$list(i),
							n,
							args));
				} else {
					return e;
				}
			})
	};
};
var $author$project$Context$cons0 = {
	j: function (e) {
		if (e.$ === 5) {
			var e0 = e.a;
			return $elm$core$Maybe$Just(e0);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	},
	m: F2(
		function (e0, e) {
			if (e.$ === 5) {
				var e1 = e.b;
				return A2($author$project$AST$Cons, e0, e1);
			} else {
				return e;
			}
		})
};
var $author$project$Context$cons1 = {
	j: function (e) {
		if (e.$ === 5) {
			var e1 = e.b;
			return $elm$core$Maybe$Just(e1);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	},
	m: F2(
		function (e1, e) {
			if (e.$ === 5) {
				var e0 = e.a;
				return A2($author$project$AST$Cons, e0, e1);
			} else {
				return e;
			}
		})
};
var $author$project$Context$hole = {
	j: function (expr) {
		return $elm$core$Maybe$Just(expr);
	},
	m: F2(
		function (_new, _v0) {
			return _new;
		})
};
var $author$project$Context$if0 = {
	j: function (e) {
		if (e.$ === 9) {
			var e0 = e.a;
			var e1 = e.b;
			var e2 = e.c;
			return $elm$core$Maybe$Just(e0);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	},
	m: F2(
		function (e0, e) {
			if (e.$ === 9) {
				var e1 = e.b;
				var e2 = e.c;
				return A3($author$project$AST$IfThenElse, e0, e1, e2);
			} else {
				return e;
			}
		})
};
var $author$project$Context$infixOp0 = {
	j: function (e) {
		if (e.$ === 8) {
			var e0 = e.b;
			return $elm$core$Maybe$Just(e0);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	},
	m: F2(
		function (e0, e) {
			if (e.$ === 8) {
				var op = e.a;
				var e1 = e.c;
				return A3($author$project$AST$InfixOp, op, e0, e1);
			} else {
				return e;
			}
		})
};
var $author$project$Context$infixOp1 = {
	j: function (e) {
		if (e.$ === 8) {
			var e1 = e.c;
			return $elm$core$Maybe$Just(e1);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	},
	m: F2(
		function (e1, e) {
			if (e.$ === 8) {
				var op = e.a;
				var e0 = e.b;
				return A3($author$project$AST$InfixOp, op, e0, e1);
			} else {
				return e;
			}
		})
};
var $author$project$Context$listItem = function (i) {
	return {
		j: function (e) {
			if (e.$ === 6) {
				var items = e.a;
				return A2(
					function ($) {
						return $.j;
					},
					$arturopala$elm_monocle$Monocle$Common$list(i),
					items);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		},
		m: F2(
			function (n, e) {
				if (e.$ === 6) {
					var items = e.a;
					return $author$project$AST$ListLit(
						A3(
							function ($) {
								return $.m;
							},
							$arturopala$elm_monocle$Monocle$Common$list(i),
							n,
							items));
				} else {
					return e;
				}
			})
	};
};
var $author$project$Context$tupleItem = function (i) {
	return {
		j: function (e) {
			if (e.$ === 7) {
				var items = e.a;
				return A2(
					function ($) {
						return $.j;
					},
					$arturopala$elm_monocle$Monocle$Common$list(i),
					items);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		},
		m: F2(
			function (n, e) {
				if (e.$ === 7) {
					var items = e.a;
					return $author$project$AST$TupleLit(
						A3(
							function ($) {
								return $.m;
							},
							$arturopala$elm_monocle$Monocle$Common$list(i),
							n,
							items));
				} else {
					return e;
				}
			})
	};
};
var $author$project$Eval$outermostRedex = F2(
	function (functions, expr) {
		var _v9 = A2($author$project$Eval$redex, functions, expr);
		if (!_v9.$) {
			return $elm$core$Maybe$Just($author$project$Context$hole);
		} else {
			return A2($author$project$Eval$outermostRedexAux, functions, expr);
		}
	});
var $author$project$Eval$outermostRedexArgs = F4(
	function (functions, proj, args, i) {
		outermostRedexArgs:
		while (true) {
			if (args.b) {
				var arg = args.a;
				var rest = args.b;
				var _v8 = A2($author$project$Eval$outermostRedex, functions, arg);
				if (!_v8.$) {
					var ctx = _v8.a;
					return $elm$core$Maybe$Just(
						A2(
							$arturopala$elm_monocle$Monocle$Optional$compose,
							proj(i),
							ctx));
				} else {
					var $temp$functions = functions,
						$temp$proj = proj,
						$temp$args = rest,
						$temp$i = i + 1;
					functions = $temp$functions;
					proj = $temp$proj;
					args = $temp$args;
					i = $temp$i;
					continue outermostRedexArgs;
				}
			} else {
				return $elm$core$Maybe$Nothing;
			}
		}
	});
var $author$project$Eval$outermostRedexAux = F2(
	function (functions, expr) {
		switch (expr.$) {
			case 5:
				var e0 = expr.a;
				var e1 = expr.b;
				var _v1 = A2($author$project$Eval$outermostRedex, functions, e0);
				if (!_v1.$) {
					var ctx = _v1.a;
					return $elm$core$Maybe$Just(
						A2($arturopala$elm_monocle$Monocle$Optional$compose, $author$project$Context$cons0, ctx));
				} else {
					var _v2 = A2($author$project$Eval$outermostRedex, functions, e1);
					if (!_v2.$) {
						var ctx = _v2.a;
						return $elm$core$Maybe$Just(
							A2($arturopala$elm_monocle$Monocle$Optional$compose, $author$project$Context$cons1, ctx));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}
			case 8:
				var op = expr.a;
				var e0 = expr.b;
				var e1 = expr.c;
				var _v3 = A2($author$project$Eval$outermostRedex, functions, e0);
				if (!_v3.$) {
					var ctx = _v3.a;
					return $elm$core$Maybe$Just(
						A2($arturopala$elm_monocle$Monocle$Optional$compose, $author$project$Context$infixOp0, ctx));
				} else {
					var _v4 = A2($author$project$Eval$outermostRedex, functions, e1);
					if (!_v4.$) {
						var ctx = _v4.a;
						return $elm$core$Maybe$Just(
							A2($arturopala$elm_monocle$Monocle$Optional$compose, $author$project$Context$infixOp1, ctx));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}
			case 0:
				var e0 = expr.a;
				var args = expr.b;
				var _v5 = A2($author$project$Eval$outermostRedex, functions, e0);
				if (!_v5.$) {
					var ctx = _v5.a;
					return $elm$core$Maybe$Just(
						A2($arturopala$elm_monocle$Monocle$Optional$compose, $author$project$Context$app0, ctx));
				} else {
					return A4($author$project$Eval$outermostRedexArgs, functions, $author$project$Context$appArg, args, 0);
				}
			case 7:
				var items = expr.a;
				return A4($author$project$Eval$outermostRedexArgs, functions, $author$project$Context$tupleItem, items, 0);
			case 6:
				var items = expr.a;
				return A4($author$project$Eval$outermostRedexArgs, functions, $author$project$Context$listItem, items, 0);
			case 9:
				var e0 = expr.a;
				var e1 = expr.b;
				var e2 = expr.c;
				var _v6 = A2($author$project$Eval$outermostRedex, functions, e0);
				if (!_v6.$) {
					var ctx = _v6.a;
					return $elm$core$Maybe$Just(
						A2($arturopala$elm_monocle$Monocle$Optional$compose, $author$project$Context$if0, ctx));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			default:
				return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Haskelite$reduceNext = F2(
	function (functions, expr) {
		return A2(
			$elm$core$Maybe$andThen,
			function (ctx) {
				return A3($author$project$Eval$redexCtx, functions, expr, ctx);
			},
			A2($author$project$Eval$outermostRedex, functions, expr));
	});
var $author$project$Haskelite$reduceUpdate = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var ctx = msg.a;
				var _v1 = A3($author$project$Eval$redexCtx, model.C, model.g, ctx);
				if (!_v1.$) {
					var _v2 = _v1.a;
					var newExpr = _v2.a;
					var info = _v2.b;
					return _Utils_update(
						model,
						{
							g: newExpr,
							l: A2(
								$elm$core$List$cons,
								_Utils_Tuple2(model.g, info),
								model.l)
						});
				} else {
					return model;
				}
			case 1:
				var _v3 = model.l;
				if (_v3.b) {
					var _v4 = _v3.a;
					var oldExpr = _v4.a;
					var info = _v4.b;
					var rest = _v3.b;
					return _Utils_update(
						model,
						{g: oldExpr, l: rest});
				} else {
					return model;
				}
			case 2:
				var _v5 = A2($author$project$Haskelite$reduceNext, model.C, model.g);
				if (!_v5.$) {
					var _v6 = _v5.a;
					var newExpr = _v6.a;
					var info = _v6.b;
					return _Utils_update(
						model,
						{
							g: newExpr,
							l: A2(
								$elm$core$List$cons,
								_Utils_Tuple2(model.g, info),
								model.l)
						});
				} else {
					return model;
				}
			case 3:
				var _v7 = $elm_community$list_extra$List$Extra$last(model.l);
				if (!_v7.$) {
					var _v8 = _v7.a;
					var expr = _v8.a;
					return _Utils_update(
						model,
						{g: expr, l: _List_Nil});
				} else {
					return model;
				}
			case 6:
				return _Utils_update(
					model,
					{I: 0});
			default:
				return model;
		}
	});
var $author$project$Haskelite$update = F2(
	function (msg, model) {
		var _v0 = model.I;
		if (_v0 === 1) {
			return _Utils_Tuple2(
				A2($author$project$Haskelite$reduceUpdate, msg, model),
				$elm$core$Platform$Cmd$none);
		} else {
			return _Utils_Tuple2(
				A2($author$project$Haskelite$editUpdate, msg, model),
				$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Haskelite$EditDecls = function (a) {
	return {$: 5, a: a};
};
var $author$project$Haskelite$EditExpr = function (a) {
	return {$: 4, a: a};
};
var $author$project$Haskelite$SaveEdits = {$: 7};
var $elm$html$Html$br = _VirtualDom_node('br');
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$Attributes$cols = function (n) {
	return A2(
		_VirtualDom_attribute,
		'cols',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 1, a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$Attributes$rows = function (n) {
	return A2(
		_VirtualDom_attribute,
		'rows',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$Attributes$size = function (n) {
	return A2(
		_VirtualDom_attribute,
		'size',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$spellcheck = $elm$html$Html$Attributes$boolProperty('spellcheck');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Haskelite$editingView = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('navbar'),
						$elm$html$Html$Events$onClick($author$project$Haskelite$SaveEdits)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Save')
					])),
				A2($elm$html$Html$br, _List_Nil, _List_Nil),
				A2(
				$elm$html$Html$input,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$placeholder('Enter an expression'),
						$elm$html$Html$Attributes$value(model.T),
						$elm$html$Html$Attributes$size(80),
						$elm$html$Html$Attributes$spellcheck(false),
						$elm$html$Html$Attributes$class('editline'),
						$elm$html$Html$Events$onInput($author$project$Haskelite$EditExpr)
					]),
				_List_Nil),
				A2($elm$html$Html$br, _List_Nil, _List_Nil),
				function () {
				var _v0 = model.O;
				if (_v0.$ === 1) {
					var msg = _v0.a;
					return A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('error')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(msg)
							]));
				} else {
					return A2($elm$html$Html$span, _List_Nil, _List_Nil);
				}
			}(),
				A2($elm$html$Html$br, _List_Nil, _List_Nil),
				A2(
				$elm$html$Html$textarea,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$value(model.S),
						$elm$html$Html$Attributes$rows(24),
						$elm$html$Html$Attributes$cols(80),
						$elm$html$Html$Attributes$spellcheck(false),
						$elm$html$Html$Events$onInput($author$project$Haskelite$EditDecls)
					]),
				_List_Nil),
				A2($elm$html$Html$br, _List_Nil, _List_Nil),
				function () {
				var _v1 = model.N;
				if (_v1.$ === 1) {
					var msg = _v1.a;
					return A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('error')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(msg)
							]));
				} else {
					return A2($elm$html$Html$span, _List_Nil, _List_Nil);
				}
			}()
			]));
};
var $author$project$Haskelite$Edit = {$: 6};
var $author$project$Haskelite$Next = {$: 2};
var $author$project$Haskelite$Previous = {$: 1};
var $author$project$Haskelite$Reset = {$: 3};
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Haskelite$isNormalForm = F2(
	function (functions, expr) {
		var _v0 = A2($author$project$Haskelite$reduceNext, functions, expr);
		if (!_v0.$) {
			return false;
		} else {
			return true;
		}
	});
var $author$project$Haskelite$lineView = function (_v0) {
	var expr = _v0.a;
	var info = _v0.b;
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('line')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				$author$project$Pretty$prettyExpr(expr)),
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('info')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(info)
					]))
			]));
};
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $author$project$Haskelite$paren = F2(
	function (b, html) {
		return b ? A2(
			$elm$html$Html$span,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('('),
					html,
					$elm$html$Html$text(')')
				])) : html;
	});
var $author$project$Haskelite$Step = function (a) {
	return {$: 0, a: a};
};
var $author$project$Haskelite$redexSpan = F4(
	function (functions, expr, ctx, elements) {
		var _v0 = A2($author$project$Eval$redex, functions, expr);
		if (!_v0.$) {
			var _v1 = _v0.a;
			var info = _v1.b;
			return A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('redex'),
						$elm$html$Html$Events$onClick(
						$author$project$Haskelite$Step(ctx))
					]),
				A2(
					$elm$core$List$cons,
					A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('info')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text(info)
							])),
					elements));
		} else {
			return A2($elm$html$Html$span, _List_Nil, elements);
		}
	});
var $author$project$Haskelite$renderExpr = F3(
	function (functions, expr, ctx) {
		return A4($author$project$Haskelite$renderExpr_, 0, functions, expr, ctx);
	});
var $author$project$Haskelite$renderExpr_ = F4(
	function (prec, functions, expr, ctx) {
		switch (expr.$) {
			case 2:
				var x = expr.a;
				return $elm$html$Html$text(
					$author$project$Pretty$isOperator(x) ? ('(' + (x + ')')) : x);
			case 3:
				var n = expr.a;
				return $elm$html$Html$text(
					$elm$core$String$fromInt(n));
			case 4:
				var b = expr.a;
				return $elm$html$Html$text(
					b ? 'True' : 'False');
			case 7:
				var args = expr.a;
				var n = $elm$core$List$length(args) - 1;
				var ctxs = A2(
					$elm$core$List$map,
					function (i) {
						return A2(
							$arturopala$elm_monocle$Monocle$Optional$compose,
							ctx,
							$author$project$Context$tupleItem(i));
					},
					A2($elm$core$List$range, 0, n));
				var items = A2(
					$elm$core$List$intersperse,
					$elm$html$Html$text(','),
					A3(
						$elm$core$List$map2,
						$author$project$Haskelite$renderExpr(functions),
						args,
						ctxs));
				return A2(
					$elm$html$Html$span,
					_List_Nil,
					_Utils_ap(
						A2(
							$elm$core$List$cons,
							$elm$html$Html$text('('),
							items),
						_List_fromArray(
							[
								$elm$html$Html$text(')')
							])));
			case 6:
				var args = expr.a;
				var n = $elm$core$List$length(args) - 1;
				var ctxs = A2(
					$elm$core$List$map,
					function (i) {
						return A2(
							$arturopala$elm_monocle$Monocle$Optional$compose,
							ctx,
							$author$project$Context$listItem(i));
					},
					A2($elm$core$List$range, 0, n));
				var items = A2(
					$elm$core$List$intersperse,
					$elm$html$Html$text(','),
					A3(
						$elm$core$List$map2,
						$author$project$Haskelite$renderExpr(functions),
						args,
						ctxs));
				return A2(
					$elm$html$Html$span,
					_List_Nil,
					_Utils_ap(
						A2(
							$elm$core$List$cons,
							$elm$html$Html$text('['),
							items),
						_List_fromArray(
							[
								$elm$html$Html$text(']')
							])));
			case 5:
				var e0 = expr.a;
				var e1 = expr.b;
				var ctx1 = A2($arturopala$elm_monocle$Monocle$Optional$compose, ctx, $author$project$Context$cons1);
				var ctx0 = A2($arturopala$elm_monocle$Monocle$Optional$compose, ctx, $author$project$Context$cons0);
				return A2(
					$author$project$Haskelite$paren,
					prec > 0,
					A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								A4($author$project$Haskelite$renderExpr_, 1, functions, e0, ctx0),
								A4(
								$author$project$Haskelite$redexSpan,
								functions,
								expr,
								ctx,
								_List_fromArray(
									[
										$elm$html$Html$text(':')
									])),
								A4($author$project$Haskelite$renderExpr_, 1, functions, e1, ctx1)
							])));
			case 8:
				var op = expr.a;
				var e0 = expr.b;
				var e1 = expr.c;
				var ctx1 = A2($arturopala$elm_monocle$Monocle$Optional$compose, ctx, $author$project$Context$infixOp1);
				var ctx0 = A2($arturopala$elm_monocle$Monocle$Optional$compose, ctx, $author$project$Context$infixOp0);
				return A2(
					$author$project$Haskelite$paren,
					prec > 0,
					A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								A4($author$project$Haskelite$renderExpr_, 1, functions, e0, ctx0),
								A4(
								$author$project$Haskelite$redexSpan,
								functions,
								expr,
								ctx,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$Pretty$formatOperator(op))
									])),
								A4($author$project$Haskelite$renderExpr_, 1, functions, e1, ctx1)
							])));
			case 0:
				var e0 = expr.a;
				var args = expr.b;
				var n = $elm$core$List$length(args) - 1;
				var ctxs = A2(
					$elm$core$List$map,
					function (i) {
						return A2(
							$arturopala$elm_monocle$Monocle$Optional$compose,
							ctx,
							$author$project$Context$appArg(i));
					},
					A2($elm$core$List$range, 0, n));
				var items = A2(
					$elm$core$List$intersperse,
					$elm$html$Html$text(' '),
					A3(
						$elm$core$List$map2,
						F2(
							function (ei, ctxi) {
								return A4($author$project$Haskelite$renderExpr_, 1, functions, ei, ctxi);
							}),
						args,
						ctxs));
				var ctx0 = A2($arturopala$elm_monocle$Monocle$Optional$compose, ctx, $author$project$Context$app0);
				return A2(
					$author$project$Haskelite$paren,
					prec > 0,
					A2(
						$elm$html$Html$span,
						_List_Nil,
						A2(
							$elm$core$List$cons,
							A4(
								$author$project$Haskelite$redexSpan,
								functions,
								expr,
								ctx,
								_List_fromArray(
									[
										A4($author$project$Haskelite$renderExpr_, 1, functions, e0, ctx0)
									])),
							A2(
								$elm$core$List$cons,
								$elm$html$Html$text(' '),
								items))));
			case 1:
				var xs = expr.a;
				var e1 = expr.b;
				return $elm$html$Html$text(
					A2($author$project$Pretty$prettyExpr_, prec, expr));
			case 9:
				var e1 = expr.a;
				var e2 = expr.b;
				var e3 = expr.c;
				return A2(
					$author$project$Haskelite$paren,
					prec > 0,
					A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								A4(
								$author$project$Haskelite$redexSpan,
								functions,
								expr,
								ctx,
								_List_fromArray(
									[
										$elm$html$Html$text('if ')
									])),
								A3(
								$author$project$Haskelite$renderExpr,
								functions,
								e1,
								A2($arturopala$elm_monocle$Monocle$Optional$compose, ctx, $author$project$Context$if0)),
								A4(
								$author$project$Haskelite$redexSpan,
								functions,
								expr,
								ctx,
								_List_fromArray(
									[
										$elm$html$Html$text(' then ')
									])),
								$elm$html$Html$text(
								$author$project$Pretty$prettyExpr(e2)),
								A4(
								$author$project$Haskelite$redexSpan,
								functions,
								expr,
								ctx,
								_List_fromArray(
									[
										$elm$html$Html$text(' else ')
									])),
								$elm$html$Html$text(
								$author$project$Pretty$prettyExpr(e3))
							])));
			default:
				var msg = expr.a;
				return A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('error')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(msg)
						]));
		}
	});
var $author$project$Haskelite$reduceView = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$span,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('navbar'),
								$elm$html$Html$Attributes$disabled(
								!$elm$core$List$isEmpty(model.l)),
								$elm$html$Html$Events$onClick($author$project$Haskelite$Edit)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Edit')
							])),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('navbar'),
								$elm$html$Html$Events$onClick($author$project$Haskelite$Reset)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Reset')
							])),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('navbar'),
								$elm$html$Html$Attributes$disabled(
								$elm$core$List$isEmpty(model.l)),
								$elm$html$Html$Events$onClick($author$project$Haskelite$Previous)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('< Prev')
							])),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('navbar'),
								$elm$html$Html$Attributes$disabled(
								A2($author$project$Haskelite$isNormalForm, model.C, model.g)),
								$elm$html$Html$Events$onClick($author$project$Haskelite$Next)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Next >')
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('lines')
					]),
				_Utils_ap(
					A2(
						$elm$core$List$map,
						$author$project$Haskelite$lineView,
						$elm$core$List$reverse(model.l)),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('current')
								]),
							_List_fromArray(
								[
									A3($author$project$Haskelite$renderExpr, model.C, model.g, $author$project$Context$hole)
								]))
						])))
			]));
};
var $author$project$Haskelite$view = function (model) {
	var _v0 = model.I;
	if (!_v0) {
		return $author$project$Haskelite$editingView(model);
	} else {
		return $author$project$Haskelite$reduceView(model);
	}
};
var $author$project$Haskelite$main = $elm$browser$Browser$element(
	{aX: $author$project$Haskelite$init, a5: $author$project$Haskelite$subscriptions, a7: $author$project$Haskelite$update, a8: $author$project$Haskelite$view});
_Platform_export({'Haskelite':{'init':$author$project$Haskelite$main(
	A2(
		$elm$json$Json$Decode$andThen,
		function (expression) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (declarations) {
					return $elm$json$Json$Decode$succeed(
						{Z: declarations, g: expression});
				},
				A2($elm$json$Json$Decode$field, 'declarations', $elm$json$Json$Decode$string));
		},
		A2($elm$json$Json$Decode$field, 'expression', $elm$json$Json$Decode$string)))(0)}});}(this));