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

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


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

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
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

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
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


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
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
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
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

	/**/
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

	/**_UNUSED/
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

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
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

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


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



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


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



/**/
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

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

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
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
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


function _Platform_export_UNUSED(exports)
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


function _Platform_export(exports)
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

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
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

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
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
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
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
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
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
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
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
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
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
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
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
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
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
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
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
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
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
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
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
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
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


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.multiline) { flags += 'm'; }
	if (options.caseInsensitive) { flags += 'i'; }

	try
	{
		return $elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return $elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		out.push(A4($elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		return replacer(A4($elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		$elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}


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
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
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
			if (t.$ === 'RBEmpty_elm_builtin') {
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
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
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
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
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
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
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
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
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
	return {$: 'Leaf', a: a};
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
	return {$: 'SubTree', a: a};
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
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
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
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
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
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
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
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
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
					if (_v1.$ === 'Nothing') {
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
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
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
		var task = _v0.a;
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
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $wernerdegroot$listzipper$List$Zipper$Zipper = F3(
	function (a, b, c) {
		return {$: 'Zipper', a: a, b: b, c: c};
	});
var $wernerdegroot$listzipper$List$Zipper$from = F3(
	function (bef, curr, aft) {
		return A3(
			$wernerdegroot$listzipper$List$Zipper$Zipper,
			$elm$core$List$reverse(bef),
			curr,
			aft);
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
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
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
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
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
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
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$WordOrPhrase$lastReviewedOn = function (wop) {
	return $elm$core$List$maximum(
		A2(
			$elm$core$List$map,
			function ($) {
				return $.timestamp;
			},
			wop.reviewHistory));
};
var $matthewsj$elm_ordering$Ordering$breakTiesWith = F4(
	function (tiebreaker, mainOrdering, x, y) {
		var _v0 = A2(mainOrdering, x, y);
		switch (_v0.$) {
			case 'LT':
				return $elm$core$Basics$LT;
			case 'GT':
				return $elm$core$Basics$GT;
			default:
				return A2(tiebreaker, x, y);
		}
	});
var $matthewsj$elm_ordering$Ordering$byFieldWith = F4(
	function (compareField, extractField, x, y) {
		return A2(
			compareField,
			extractField(x),
			extractField(y));
	});
var $matthewsj$elm_ordering$Ordering$natural = $elm$core$Basics$compare;
var $matthewsj$elm_ordering$Ordering$byField = $matthewsj$elm_ordering$Ordering$byFieldWith($matthewsj$elm_ordering$Ordering$natural);
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
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
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Basics$not = _Basics_not;
var $elm_community$list_extra$List$Extra$filterNot = F2(
	function (pred, list) {
		return A2(
			$elm$core$List$filter,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, pred),
			list);
	});
var $elm$core$String$fromList = _String_fromList;
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return A2($elm$core$Dict$member, key, dict);
	});
var $elm$core$Set$Set_elm_builtin = function (a) {
	return {$: 'Set_elm_builtin', a: a};
};
var $elm$core$Set$empty = $elm$core$Set$Set_elm_builtin($elm$core$Dict$empty);
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0.a;
		return $elm$core$Set$Set_elm_builtin(
			A3($elm$core$Dict$insert, key, _Utils_Tuple0, dict));
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $author$project$WordOrPhrase$tashkylSet = $elm$core$Set$fromList(
	_List_fromArray(
		[
			_Utils_chr('??'),
			_Utils_chr('??'),
			_Utils_chr('??'),
			_Utils_chr('??'),
			_Utils_chr('??')
		]));
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $author$project$WordOrPhrase$splitOffTashkyl = function (string) {
	var chars = $elm$core$String$toList(string);
	var justTashkyl = A2(
		$elm$core$List$filter,
		function (ch) {
			return A2($elm$core$Set$member, ch, $author$project$WordOrPhrase$tashkylSet);
		},
		chars);
	var withoutTashkyl = $elm$core$String$fromList(
		A2(
			$elm_community$list_extra$List$Extra$filterNot,
			function (ch) {
				return A2($elm$core$Set$member, ch, $author$project$WordOrPhrase$tashkylSet);
			},
			chars));
	return _Utils_Tuple2(withoutTashkyl, justTashkyl);
};
var $author$project$WordOrPhrase$removeTashkyl = A2($elm$core$Basics$composeR, $author$project$WordOrPhrase$splitOffTashkyl, $elm$core$Tuple$first);
var $author$project$WordOrPhrase$get = function (wopKey) {
	return $elm$core$Dict$get(
		$author$project$WordOrPhrase$removeTashkyl(wopKey));
};
var $author$project$WordDisplay$getWord = function (wdt) {
	switch (wdt.$) {
		case 'DisplayWord':
			var w = wdt.a;
			return $elm$core$Maybe$Just(w);
		case 'DisplayWordOfPhrase':
			var w = wdt.a;
			return $elm$core$Maybe$Just(w);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $author$project$WordDisplay$DisplayNonWord = function (a) {
	return {$: 'DisplayNonWord', a: a};
};
var $author$project$WordDisplay$DisplayWord = function (a) {
	return {$: 'DisplayWord', a: a};
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {index: index, match: match, number: number, submatches: submatches};
	});
var $elm$regex$Regex$find = _Regex_findAtMost(_Regex_infinity);
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$fromString = function (string) {
	return A2(
		$elm$regex$Regex$fromStringWith,
		{caseInsensitive: false, multiline: false},
		string);
};
var $elm$regex$Regex$never = _Regex_never;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$WordDisplay$markWordCharsFromNonWordChars = function (lineOfText) {
	var arabicNumerals = '????????????????????';
	var rxString = ' *():.???!,?????=\\-' + arabicNumerals;
	var nonWordDetectorRx = A2(
		$elm$core$Maybe$withDefault,
		$elm$regex$Regex$never,
		$elm$regex$Regex$fromString('([' + (rxString + (']*)' + ('([^' + (rxString + ']*)'))))));
	return A2(
		$elm$core$List$concatMap,
		function (match) {
			var _v0 = match.submatches;
			if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
				var mnonWordChars = _v0.a;
				var _v1 = _v0.b;
				var mwordChars = _v1.a;
				return _Utils_ap(
					function () {
						if (mnonWordChars.$ === 'Just') {
							var nonWordChars = mnonWordChars.a;
							return _List_fromArray(
								[
									$author$project$WordDisplay$DisplayNonWord(nonWordChars)
								]);
						} else {
							return _List_Nil;
						}
					}(),
					function () {
						if (mwordChars.$ === 'Just') {
							var wordChars = mwordChars.a;
							return _List_fromArray(
								[
									$author$project$WordDisplay$DisplayWord(wordChars)
								]);
						} else {
							return _List_Nil;
						}
					}());
			} else {
				return _List_Nil;
			}
		},
		A2($elm$regex$Regex$find, nonWordDetectorRx, lineOfText));
};
var $author$project$Lesson$getWordsFromString = function (str) {
	return A2(
		$elm$core$List$filterMap,
		$author$project$WordDisplay$getWord,
		$author$project$WordDisplay$markWordCharsFromNonWordChars(str));
};
var $elm$core$String$trim = _String_trim;
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
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $elm_community$list_extra$List$Extra$uniqueHelp = F4(
	function (f, existing, remaining, accumulator) {
		uniqueHelp:
		while (true) {
			if (!remaining.b) {
				return $elm$core$List$reverse(accumulator);
			} else {
				var first = remaining.a;
				var rest = remaining.b;
				var computedFirst = f(first);
				if (A2($elm$core$List$member, computedFirst, existing)) {
					var $temp$f = f,
						$temp$existing = existing,
						$temp$remaining = rest,
						$temp$accumulator = accumulator;
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				} else {
					var $temp$f = f,
						$temp$existing = A2($elm$core$List$cons, computedFirst, existing),
						$temp$remaining = rest,
						$temp$accumulator = A2($elm$core$List$cons, first, accumulator);
					f = $temp$f;
					existing = $temp$existing;
					remaining = $temp$remaining;
					accumulator = $temp$accumulator;
					continue uniqueHelp;
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$unique = function (list) {
	return A4($elm_community$list_extra$List$Extra$uniqueHelp, $elm$core$Basics$identity, _List_Nil, list, _List_Nil);
};
var $author$project$Lesson$getWords = function (lessonText) {
	return $elm_community$list_extra$List$Extra$unique(
		A2(
			$elm$core$List$filter,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, $elm$core$String$isEmpty),
			A2(
				$elm$core$List$map,
				$elm$core$String$trim,
				$author$project$Lesson$getWordsFromString(lessonText))));
};
var $author$project$Lesson$getWops = F2(
	function (wops, lessonText) {
		return A2(
			$elm$core$List$filterMap,
			function (word) {
				return A2($author$project$WordOrPhrase$get, word, wops);
			},
			$author$project$Lesson$getWords(lessonText));
	});
var $author$project$WordOrPhrase$wordOrPhraseToKey = function (wordOrPhrase) {
	return A2($elm$core$String$join, ' ', wordOrPhrase);
};
var $author$project$WordOrPhrase$key = function (_v0) {
	var wordOrPhrase = _v0.wordOrPhrase;
	return $author$project$WordOrPhrase$wordOrPhraseToKey(wordOrPhrase);
};
var $author$project$WordOrPhrase$intoDict = function (wops) {
	return $elm$core$Dict$fromList(
		A2(
			$elm$core$List$map,
			function (wop) {
				return _Utils_Tuple2(
					$author$project$WordOrPhrase$removeTashkyl(
						$author$project$WordOrPhrase$key(wop)),
					wop);
			},
			wops));
};
var $matthewsj$elm_ordering$Ordering$reverse = F3(
	function (ordering, x, y) {
		var _v0 = A2(ordering, x, y);
		switch (_v0.$) {
			case 'LT':
				return $elm$core$Basics$GT;
			case 'EQ':
				return $elm$core$Basics$EQ;
			default:
				return $elm$core$Basics$LT;
		}
	});
var $elm$core$List$sortWith = _List_sortWith;
var $author$project$DueForReview$lessonsByReviewDensity = F3(
	function (allWops, lessons, dueWops) {
		return A2(
			$elm$core$List$sortWith,
			$matthewsj$elm_ordering$Ordering$reverse(
				A2(
					$matthewsj$elm_ordering$Ordering$breakTiesWith,
					$matthewsj$elm_ordering$Ordering$byField(
						function ($) {
							return $.totalDueInLesson;
						}),
					$matthewsj$elm_ordering$Ordering$byField(
						function ($) {
							return $.densityRatio;
						}))),
			A2(
				$elm$core$List$map,
				function (_v0) {
					var title = _v0.a;
					var lesson = _v0.b;
					var dueWopsInLesson = A2(
						$author$project$Lesson$getWops,
						$author$project$WordOrPhrase$intoDict(dueWops),
						lesson.text);
					var allWopsInLesson = A2($author$project$Lesson$getWops, allWops, lesson.text);
					var _v1 = _Utils_Tuple2(
						$elm$core$List$length(dueWopsInLesson),
						$elm$core$List$length(allWopsInLesson));
					var totalDueInLesson = _v1.a;
					var totalWopsInLesson = _v1.b;
					var ratio = totalDueInLesson / totalWopsInLesson;
					return {densityRatio: ratio, dueWopsInLesson: dueWopsInLesson, lessonText: lesson.text, lessonTitle: title, totalDueInLesson: totalDueInLesson};
				},
				$elm$core$Dict$toList(lessons)));
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
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
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$DueForReview$wopsDueForReview = function (wops) {
	return A2(
		$elm$core$List$map,
		$elm$core$Tuple$first,
		A2(
			$elm$core$List$sortBy,
			$elm$core$Tuple$second,
			A2(
				$elm$core$List$map,
				function (wop) {
					return _Utils_Tuple2(
						wop,
						A2(
							$elm$core$Maybe$withDefault,
							0,
							$author$project$WordOrPhrase$lastReviewedOn(wop)));
				},
				wops)));
};
var $author$project$DueForReview$getLessonsDueForReview = function (model) {
	var oneDay = ((24 * 60) * 60) * 1000;
	var dueForReviewList = A2(
		$elm$core$List$take,
		100,
		A2(
			$elm$core$List$filter,
			function (wop) {
				return A2(
					$elm$core$Maybe$withDefault,
					true,
					A2(
						$elm$core$Maybe$map,
						function (lastReviewed) {
							return (wop.familiarityLevel === 1) ? (_Utils_cmp(
								$elm$time$Time$posixToMillis(model.tick) - lastReviewed,
								oneDay) > 0) : (_Utils_cmp(
								$elm$time$Time$posixToMillis(model.tick) - lastReviewed,
								4 * oneDay) > 0);
						},
						$author$project$WordOrPhrase$lastReviewedOn(wop)));
			},
			A2(
				$elm$core$List$filter,
				function (_v0) {
					var familiarityLevel = _v0.familiarityLevel;
					return familiarityLevel <= 2;
				},
				$author$project$DueForReview$wopsDueForReview(
					$elm$core$Dict$values(model.wops)))));
	return A3($author$project$DueForReview$lessonsByReviewDensity, model.wops, model.lessons, dueForReviewList);
};
var $author$project$WordOrPhrase$makeWOP = F2(
	function (wordOrPhrase, definition) {
		return {
			definitions: _List_fromArray(
				[definition]),
			familiarityLevel: 1,
			notes: '',
			reviewHistory: _List_Nil,
			romanization: '',
			tags: _List_Nil,
			wordOrPhrase: wordOrPhrase
		};
	});
var $author$project$EnterNewWops$initWOP = A2($author$project$WordOrPhrase$makeWOP, _List_Nil, '');
var $author$project$EnterNewWops$init = {createdWops: _List_Nil, newWop: $author$project$EnterNewWops$initWOP, viewingEnterNewWops: false};
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Main$init = function (_v0) {
	var tick = _v0.tick;
	var lessons = _v0.lessons;
	var wops = _v0.wops;
	var lessonTranslations = _v0.lessonTranslations;
	var newWopFlashcards = _v0.newWopFlashcards;
	return _Utils_Tuple2(
		function (m) {
			return _Utils_update(
				m,
				{
					lessonsDueForReview: $author$project$DueForReview$getLessonsDueForReview(m)
				});
		}(
			{
				drawInLesson: false,
				drawRectangleKalimniBuffer: $elm$core$Maybe$Nothing,
				enterNewWops: $author$project$EnterNewWops$init,
				flashcardFlipped: false,
				hoveredWord: '',
				lastKalimniEvent: $elm$core$Maybe$Nothing,
				lessonFlashcards: $elm$core$Maybe$Nothing,
				lessonTranslations: $elm$core$Dict$fromList(lessonTranslations),
				lessons: $elm$core$Dict$fromList(lessons),
				lessonsDueForReview: _List_Nil,
				mouseDownWord: _Utils_Tuple3(0, 0, ''),
				newLessonText: '',
				newLessonTitle: '',
				newWopDefinition: '',
				newWopFlashcards: A2(
					$elm$core$Maybe$map,
					function (_v1) {
						var before = _v1.before;
						var current = _v1.current;
						var after = _v1.after;
						return A3($wernerdegroot$listzipper$List$Zipper$from, before, current, after);
					},
					newWopFlashcards),
				onFlashcardPage: false,
				selectedKalimniLessonPage: $elm$core$Maybe$Just(
					{currentScaling: 1.0, naturalHeight: 400, naturalWidth: 1000, textBoxes: _List_Nil}),
				selectedLesson: '',
				selectedWop: '',
				selectedWopTagsBuffer: '',
				startingRectangleCoordinate: $elm$core$Maybe$Nothing,
				tick: $elm$time$Time$millisToPosix(tick),
				wops: $elm$core$Dict$fromList(wops)
			}),
		$elm$core$Platform$Cmd$none);
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$GotKalimniEvent = function (a) {
	return {$: 'GotKalimniEvent', a: a};
};
var $author$project$Main$Tick = function (a) {
	return {$: 'Tick', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 'Nothing') {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.processes;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.taggers);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $author$project$Main$KeyPress = function (a) {
	return {$: 'KeyPress', a: a};
};
var $author$project$Main$keyDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Main$KeyPress,
	A2($elm$json$Json$Decode$field, 'key', $elm$json$Json$Decode$string));
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onKeyPress = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'keypress');
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $author$project$Main$receiveKalimniEvents = _Platform_incomingPort(
	'receiveKalimniEvents',
	A2(
		$elm$json$Json$Decode$andThen,
		function (width) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (relativeY) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (relativeX) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (naturalWidth) {
									return A2(
										$elm$json$Json$Decode$andThen,
										function (naturalHeight) {
											return A2(
												$elm$json$Json$Decode$andThen,
												function (height) {
													return $elm$json$Json$Decode$succeed(
														{height: height, naturalHeight: naturalHeight, naturalWidth: naturalWidth, relativeX: relativeX, relativeY: relativeY, width: width});
												},
												A2($elm$json$Json$Decode$field, 'height', $elm$json$Json$Decode$int));
										},
										A2($elm$json$Json$Decode$field, 'naturalHeight', $elm$json$Json$Decode$int));
								},
								A2($elm$json$Json$Decode$field, 'naturalWidth', $elm$json$Json$Decode$int));
						},
						A2($elm$json$Json$Decode$field, 'relativeX', $elm$json$Json$Decode$float));
				},
				A2($elm$json$Json$Decode$field, 'relativeY', $elm$json$Json$Decode$float));
		},
		A2($elm$json$Json$Decode$field, 'width', $elm$json$Json$Decode$int)));
var $author$project$Main$subscriptions = function (_v0) {
	var drawInLesson = _v0.drawInLesson;
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				$elm$browser$Browser$Events$onKeyPress($author$project$Main$keyDecoder),
				drawInLesson ? $author$project$Main$receiveKalimniEvents($author$project$Main$GotKalimniEvent) : $elm$core$Platform$Sub$none,
				A2($elm$time$Time$every, 1000, $author$project$Main$Tick)
			]));
};
var $author$project$Main$BackendAudioUpdated = function (a) {
	return {$: 'BackendAudioUpdated', a: a};
};
var $author$project$Main$GetCurrentTimeAndThen = function (a) {
	return {$: 'GetCurrentTimeAndThen', a: a};
};
var $author$project$Main$MakeNewWopFlashcardSet = F2(
	function (a, b) {
		return {$: 'MakeNewWopFlashcardSet', a: a, b: b};
	});
var $author$project$Main$SelectWOP = F2(
	function (a, b) {
		return {$: 'SelectWOP', a: a, b: b};
	});
var $author$project$Main$SetSelectedWOPFamiliarityLevel = F2(
	function (a, b) {
		return {$: 'SetSelectedWOPFamiliarityLevel', a: a, b: b};
	});
var $author$project$Flashcard$addHistoryEntry = F3(
	function (timestamp, remembered, flashcard) {
		return _Utils_update(
			flashcard,
			{
				history: A2(
					$elm$core$List$cons,
					{
						remembered: remembered,
						timestamp: $elm$time$Time$posixToMillis(timestamp)
					},
					flashcard.history)
			});
	});
var $elm$core$List$partition = F2(
	function (pred, list) {
		var step = F2(
			function (x, _v0) {
				var trues = _v0.a;
				var falses = _v0.b;
				return pred(x) ? _Utils_Tuple2(
					A2($elm$core$List$cons, x, trues),
					falses) : _Utils_Tuple2(
					trues,
					A2($elm$core$List$cons, x, falses));
			});
		return A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			list);
	});
var $elm_community$list_extra$List$Extra$gatherWith = F2(
	function (testFn, list) {
		var helper = F2(
			function (scattered, gathered) {
				helper:
				while (true) {
					if (!scattered.b) {
						return $elm$core$List$reverse(gathered);
					} else {
						var toGather = scattered.a;
						var population = scattered.b;
						var _v1 = A2(
							$elm$core$List$partition,
							testFn(toGather),
							population);
						var gathering = _v1.a;
						var remaining = _v1.b;
						var $temp$scattered = remaining,
							$temp$gathered = A2(
							$elm$core$List$cons,
							_Utils_Tuple2(toGather, gathering),
							gathered);
						scattered = $temp$scattered;
						gathered = $temp$gathered;
						continue helper;
					}
				}
			});
		return A2(helper, list, _List_Nil);
	});
var $elm_community$list_extra$List$Extra$gatherEqualsBy = F2(
	function (extract, list) {
		return A2(
			$elm_community$list_extra$List$Extra$gatherWith,
			F2(
				function (a, b) {
					return _Utils_eq(
						extract(a),
						extract(b));
				}),
			list);
	});
var $author$project$WordOrPhrase$addReviewTime = F3(
	function (time, lessonId, wop) {
		var cleanupExcessInformation = function (history) {
			return A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2(
					$elm_community$list_extra$List$Extra$gatherEqualsBy,
					function ($) {
						return $.lessonId;
					},
					history));
		};
		return _Utils_update(
			wop,
			{
				reviewHistory: cleanupExcessInformation(
					A2(
						$elm$core$List$cons,
						{
							lessonId: $elm$core$Maybe$Just(lessonId),
							timestamp: $elm$time$Time$posixToMillis(time)
						},
						wop.reviewHistory))
			});
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Main$autofocusId = _Platform_outgoingPort('autofocusId', $elm$json$Json$Encode$string);
var $wernerdegroot$listzipper$List$Zipper$after = function (_v0) {
	var rs = _v0.c;
	return rs;
};
var $wernerdegroot$listzipper$List$Zipper$before = function (_v0) {
	var ls = _v0.a;
	return $elm$core$List$reverse(ls);
};
var $wernerdegroot$listzipper$List$Zipper$current = function (_v0) {
	var x = _v0.b;
	return x;
};
var $author$project$Utils$deconstructZipper = function (z) {
	return {
		after: $wernerdegroot$listzipper$List$Zipper$after(z),
		before: $wernerdegroot$listzipper$List$Zipper$before(z),
		current: $wernerdegroot$listzipper$List$Zipper$current(z)
	};
};
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
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var $elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var $elm$http$Http$Timeout_ = {$: 'Timeout_'};
var $elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
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
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
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
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
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
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
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
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
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
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
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
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
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
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
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
				if (_v4.$ === 'RBNode_elm_builtin') {
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
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
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
						if (_v7.$ === 'RBNode_elm_builtin') {
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
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
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
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var $elm$http$Http$NetworkError = {$: 'NetworkError'};
var $elm$http$Http$Timeout = {$: 'Timeout'};
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 'NetworkError_':
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectString = function (toMsg) {
	return A2(
		$elm$http$Http$expectStringResponse,
		toMsg,
		$elm$http$Http$resolve($elm$core$Result$Ok));
};
var $author$project$Main$splitIntoCleanLines = function (text) {
	var uniformSpacing = function (line) {
		return A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$filter,
				A2($elm$core$Basics$composeL, $elm$core$Basics$not, $elm$core$String$isEmpty),
				A2($elm$core$String$split, ' ', line)));
	};
	return A2(
		$elm$core$List$map,
		uniformSpacing,
		A2(
			$elm$core$List$filter,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, $elm$core$String$isEmpty),
			A2(
				$elm$core$List$map,
				$elm$core$String$trim,
				A2($elm$core$String$split, '\n', text))));
};
var $author$project$Main$unsplitFromCleanLines = function (cleanLines) {
	return A2($elm$core$String$join, '\n', cleanLines);
};
var $author$project$Main$extractSentences = function (text) {
	return A2(
		$elm$core$List$map,
		function ($) {
			return $.match;
		},
		A2(
			$elm$regex$Regex$find,
			A2(
				$elm$core$Maybe$withDefault,
				$elm$regex$Regex$never,
				$elm$regex$Regex$fromString('[^.?!??]+[.!???]+[\\])\'\"`??????]*|.+')),
			$author$project$Main$unsplitFromCleanLines(
				$author$project$Main$splitIntoCleanLines(text))));
};
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm_community$list_extra$List$Extra$isSubsequenceOf = F2(
	function (subseq, list) {
		isSubsequenceOf:
		while (true) {
			var _v0 = _Utils_Tuple2(subseq, list);
			if (!_v0.a.b) {
				return true;
			} else {
				if (!_v0.b.b) {
					return false;
				} else {
					var _v1 = _v0.a;
					var x = _v1.a;
					var xs = _v1.b;
					var _v2 = _v0.b;
					var y = _v2.a;
					var ys = _v2.b;
					if (_Utils_eq(x, y)) {
						var $temp$subseq = xs,
							$temp$list = ys;
						subseq = $temp$subseq;
						list = $temp$list;
						continue isSubsequenceOf;
					} else {
						var $temp$subseq = subseq,
							$temp$list = ys;
						subseq = $temp$subseq;
						list = $temp$list;
						continue isSubsequenceOf;
					}
				}
			}
		}
	});
var $author$project$WordOrPhrase$tashkylEquivalent = F2(
	function (wop1, wop2) {
		var _v0 = $author$project$WordOrPhrase$splitOffTashkyl(wop2);
		var wt2 = _v0.a;
		var jt2 = _v0.b;
		var _v1 = $author$project$WordOrPhrase$splitOffTashkyl(wop1);
		var wt1 = _v1.a;
		var jt1 = _v1.b;
		return _Utils_eq(wt1, wt2) && ($elm$core$List$isEmpty(jt1) || ($elm$core$List$isEmpty(jt2) || (A2($elm_community$list_extra$List$Extra$isSubsequenceOf, jt1, jt2) || A2($elm_community$list_extra$List$Extra$isSubsequenceOf, jt2, jt1))));
	});
var $author$project$Main$filterSentencesContainingWop = F2(
	function (sentences, wop) {
		return A2(
			$elm$core$List$filter,
			function (sentence) {
				return A2(
					$elm$core$List$any,
					$author$project$WordOrPhrase$tashkylEquivalent(
						$author$project$WordOrPhrase$key(wop)),
					$author$project$Lesson$getWords(sentence));
			},
			sentences);
	});
var $wernerdegroot$listzipper$List$Zipper$fromList = function (xs) {
	if (!xs.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var y = xs.a;
		var ys = xs.b;
		return $elm$core$Maybe$Just(
			A3($wernerdegroot$listzipper$List$Zipper$Zipper, _List_Nil, y, ys));
	}
};
var $elm$core$Basics$ge = _Utils_ge;
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm_community$list_extra$List$Extra$getAt = F2(
	function (idx, xs) {
		return (idx < 0) ? $elm$core$Maybe$Nothing : $elm$core$List$head(
			A2($elm$core$List$drop, idx, xs));
	});
var $elm_community$maybe_extra$Maybe$Extra$isNothing = function (m) {
	if (m.$ === 'Nothing') {
		return true;
	} else {
		return false;
	}
};
var $author$project$Lesson$getUnidentifiedWordsInLesson = F2(
	function (lessonText, wops) {
		return A2(
			$elm$core$List$filter,
			function (word) {
				return $elm_community$maybe_extra$Maybe$Extra$isNothing(
					A2($author$project$WordOrPhrase$get, word, wops));
			},
			$author$project$Lesson$getWords(lessonText));
	});
var $author$project$Utils$impure = F2(
	function (model, effect) {
		return _Utils_Tuple2(
			model,
			effect(model));
	});
var $author$project$WordOrPhrase$insert = function (wopKey) {
	return $elm$core$Dict$insert(
		$author$project$WordOrPhrase$removeTashkyl(wopKey));
};
var $elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2($elm$json$Json$Encode$encode, 0, value));
};
var $author$project$WordOrPhrase$keyIsPhrase = function (key_) {
	return $elm$core$List$length(
		A2($elm$core$String$split, ' ', key_)) > 1;
};
var $elm$core$Debug$log = _Debug_log;
var $author$project$Main$makeLesson = function (text) {
	return {audioFileType: 'wav', text: text};
};
var $author$project$Flashcard$makeflashcards = $elm$core$List$map(
	function (x) {
		return {data: x, history: _List_Nil};
	});
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $wernerdegroot$listzipper$List$Zipper$next = function (_v0) {
	var ls = _v0.a;
	var x = _v0.b;
	var rs = _v0.c;
	if (!rs.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var y = rs.a;
		var ys = rs.b;
		return $elm$core$Maybe$Just(
			A3(
				$wernerdegroot$listzipper$List$Zipper$Zipper,
				A2($elm$core$List$cons, x, ls),
				y,
				ys));
	}
};
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $elm$http$Http$Request = function (a) {
	return {$: 'Request', a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {reqs: reqs, subs: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (cmd.$ === 'Cancel') {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 'Nothing') {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.tracker;
							if (_v4.$ === 'Nothing') {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.reqs));
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.subs)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 'Cancel', a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (cmd.$ === 'Cancel') {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					allowCookiesFromOtherDomains: r.allowCookiesFromOtherDomains,
					body: r.body,
					expect: A2(_Http_mapExpect, func, r.expect),
					headers: r.headers,
					method: r.method,
					timeout: r.timeout,
					tracker: r.tracker,
					url: r.url
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 'MySub', a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{allowCookiesFromOtherDomains: false, body: r.body, expect: r.expect, headers: r.headers, method: r.method, timeout: r.timeout, tracker: r.tracker, url: r.url}));
};
var $elm$http$Http$post = function (r) {
	return $elm$http$Http$request(
		{body: r.body, expect: r.expect, headers: _List_Nil, method: 'POST', timeout: $elm$core$Maybe$Nothing, tracker: $elm$core$Maybe$Nothing, url: r.url});
};
var $author$project$Utils$pure = function (model) {
	return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
};
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Main$saveLocalStorageToClipboard = _Platform_outgoingPort(
	'saveLocalStorageToClipboard',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $elm_community$list_extra$List$Extra$removeAt = F2(
	function (index, l) {
		if (index < 0) {
			return l;
		} else {
			var _v0 = A2($elm$core$List$drop, index, l);
			if (!_v0.b) {
				return l;
			} else {
				var rest = _v0.b;
				return _Utils_ap(
					A2($elm$core$List$take, index, l),
					rest);
			}
		}
	});
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm_community$list_extra$List$Extra$updateAt = F3(
	function (index, fn, list) {
		if (index < 0) {
			return list;
		} else {
			var tail = A2($elm$core$List$drop, index, list);
			var head = A2($elm$core$List$take, index, list);
			if (tail.b) {
				var x = tail.a;
				var xs = tail.b;
				return _Utils_ap(
					head,
					A2(
						$elm$core$List$cons,
						fn(x),
						xs));
			} else {
				return list;
			}
		}
	});
var $elm_community$list_extra$List$Extra$setAt = F2(
	function (index, value) {
		return A2(
			$elm_community$list_extra$List$Extra$updateAt,
			index,
			$elm$core$Basics$always(value));
	});
var $author$project$WordOrPhrase$setDefinition = F3(
	function (defNumber, definition, wop) {
		return _Utils_eq(
			defNumber,
			$elm$core$List$length(wop.definitions)) ? _Utils_update(
			wop,
			{
				definitions: _Utils_ap(
					wop.definitions,
					_List_fromArray(
						[definition]))
			}) : (((!(!defNumber)) && (_Utils_eq(
			defNumber,
			$elm$core$List$length(wop.definitions) - 1) && (definition === ''))) ? _Utils_update(
			wop,
			{
				definitions: A2($elm_community$list_extra$List$Extra$removeAt, defNumber, wop.definitions)
			}) : _Utils_update(
			wop,
			{
				definitions: A3($elm_community$list_extra$List$Extra$setAt, defNumber, definition, wop.definitions)
			}));
	});
var $author$project$WordOrPhrase$setFamiliarityLevel = F2(
	function (level, wop) {
		return ((level >= 1) && (level <= 4)) ? $elm$core$Maybe$Just(
			_Utils_update(
				wop,
				{familiarityLevel: level})) : $elm$core$Maybe$Nothing;
	});
var $author$project$WordOrPhrase$setNotes = F2(
	function (notes, wop) {
		return _Utils_update(
			wop,
			{notes: notes});
	});
var $elm_community$string_extra$String$Extra$regexFromString = A2(
	$elm$core$Basics$composeR,
	$elm$regex$Regex$fromString,
	$elm$core$Maybe$withDefault($elm$regex$Regex$never));
var $elm$regex$Regex$replace = _Regex_replaceAtMost(_Regex_infinity);
var $elm_community$string_extra$String$Extra$clean = function (string) {
	return $elm$core$String$trim(
		A3(
			$elm$regex$Regex$replace,
			$elm_community$string_extra$String$Extra$regexFromString('\\s\\s+'),
			$elm$core$Basics$always(' '),
			string));
};
var $author$project$WordOrPhrase$stringToTags = function (tagString) {
	return $elm$core$String$isEmpty(tagString) ? _List_Nil : A2(
		$elm_community$list_extra$List$Extra$filterNot,
		$elm$core$String$isEmpty,
		A2(
			$elm$core$List$map,
			$elm_community$string_extra$String$Extra$clean,
			A2($elm$core$String$split, ',', tagString)));
};
var $author$project$WordOrPhrase$setTags = F2(
	function (tagString, wop) {
		return _Utils_update(
			wop,
			{
				tags: $author$project$WordOrPhrase$stringToTags(tagString)
			});
	});
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $author$project$Main$storeLessonTranslations = _Platform_outgoingPort(
	'storeLessonTranslations',
	$elm$json$Json$Encode$list(
		function ($) {
			var a = $.a;
			var b = $.b;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string(a),
						$elm$json$Json$Encode$string(b)
					]));
		}));
var $author$project$Main$storeLessons = _Platform_outgoingPort(
	'storeLessons',
	$elm$json$Json$Encode$list(
		function ($) {
			var a = $.a;
			var b = $.b;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string(a),
						function ($) {
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'audioFileType',
									$elm$json$Json$Encode$string($.audioFileType)),
									_Utils_Tuple2(
									'text',
									$elm$json$Json$Encode$string($.text))
								]));
					}(b)
					]));
		}));
var $elm$core$Maybe$destruct = F3(
	function (_default, func, maybe) {
		if (maybe.$ === 'Just') {
			var a = maybe.a;
			return func(a);
		} else {
			return _default;
		}
	});
var $elm$json$Json$Encode$int = _Json_wrap;
var $author$project$Main$storeNewWopFlashcards = _Platform_outgoingPort(
	'storeNewWopFlashcards',
	function ($) {
		return A3(
			$elm$core$Maybe$destruct,
			$elm$json$Json$Encode$null,
			function ($) {
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'after',
							$elm$json$Json$Encode$list(
								function ($) {
									return $elm$json$Json$Encode$object(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'definitions',
												$elm$json$Json$Encode$list($elm$json$Json$Encode$string)($.definitions)),
												_Utils_Tuple2(
												'familiarityLevel',
												$elm$json$Json$Encode$int($.familiarityLevel)),
												_Utils_Tuple2(
												'notes',
												$elm$json$Json$Encode$string($.notes)),
												_Utils_Tuple2(
												'reviewHistory',
												$elm$json$Json$Encode$list(
													function ($) {
														return $elm$json$Json$Encode$object(
															_List_fromArray(
																[
																	_Utils_Tuple2(
																	'lessonId',
																	function ($) {
																		return A3($elm$core$Maybe$destruct, $elm$json$Json$Encode$null, $elm$json$Json$Encode$string, $);
																	}($.lessonId)),
																	_Utils_Tuple2(
																	'timestamp',
																	$elm$json$Json$Encode$int($.timestamp))
																]));
													})($.reviewHistory)),
												_Utils_Tuple2(
												'romanization',
												$elm$json$Json$Encode$string($.romanization)),
												_Utils_Tuple2(
												'tags',
												$elm$json$Json$Encode$list($elm$json$Json$Encode$string)($.tags)),
												_Utils_Tuple2(
												'wordOrPhrase',
												$elm$json$Json$Encode$list($elm$json$Json$Encode$string)($.wordOrPhrase))
											]));
								})($.after)),
							_Utils_Tuple2(
							'before',
							$elm$json$Json$Encode$list(
								function ($) {
									return $elm$json$Json$Encode$object(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'definitions',
												$elm$json$Json$Encode$list($elm$json$Json$Encode$string)($.definitions)),
												_Utils_Tuple2(
												'familiarityLevel',
												$elm$json$Json$Encode$int($.familiarityLevel)),
												_Utils_Tuple2(
												'notes',
												$elm$json$Json$Encode$string($.notes)),
												_Utils_Tuple2(
												'reviewHistory',
												$elm$json$Json$Encode$list(
													function ($) {
														return $elm$json$Json$Encode$object(
															_List_fromArray(
																[
																	_Utils_Tuple2(
																	'lessonId',
																	function ($) {
																		return A3($elm$core$Maybe$destruct, $elm$json$Json$Encode$null, $elm$json$Json$Encode$string, $);
																	}($.lessonId)),
																	_Utils_Tuple2(
																	'timestamp',
																	$elm$json$Json$Encode$int($.timestamp))
																]));
													})($.reviewHistory)),
												_Utils_Tuple2(
												'romanization',
												$elm$json$Json$Encode$string($.romanization)),
												_Utils_Tuple2(
												'tags',
												$elm$json$Json$Encode$list($elm$json$Json$Encode$string)($.tags)),
												_Utils_Tuple2(
												'wordOrPhrase',
												$elm$json$Json$Encode$list($elm$json$Json$Encode$string)($.wordOrPhrase))
											]));
								})($.before)),
							_Utils_Tuple2(
							'current',
							function ($) {
								return $elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'definitions',
											$elm$json$Json$Encode$list($elm$json$Json$Encode$string)($.definitions)),
											_Utils_Tuple2(
											'familiarityLevel',
											$elm$json$Json$Encode$int($.familiarityLevel)),
											_Utils_Tuple2(
											'notes',
											$elm$json$Json$Encode$string($.notes)),
											_Utils_Tuple2(
											'reviewHistory',
											$elm$json$Json$Encode$list(
												function ($) {
													return $elm$json$Json$Encode$object(
														_List_fromArray(
															[
																_Utils_Tuple2(
																'lessonId',
																function ($) {
																	return A3($elm$core$Maybe$destruct, $elm$json$Json$Encode$null, $elm$json$Json$Encode$string, $);
																}($.lessonId)),
																_Utils_Tuple2(
																'timestamp',
																$elm$json$Json$Encode$int($.timestamp))
															]));
												})($.reviewHistory)),
											_Utils_Tuple2(
											'romanization',
											$elm$json$Json$Encode$string($.romanization)),
											_Utils_Tuple2(
											'tags',
											$elm$json$Json$Encode$list($elm$json$Json$Encode$string)($.tags)),
											_Utils_Tuple2(
											'wordOrPhrase',
											$elm$json$Json$Encode$list($elm$json$Json$Encode$string)($.wordOrPhrase))
										]));
							}($.current))
						]));
			},
			$);
	});
var $author$project$Main$storeWops = _Platform_outgoingPort(
	'storeWops',
	$elm$json$Json$Encode$list(
		function ($) {
			var a = $.a;
			var b = $.b;
			return A2(
				$elm$json$Json$Encode$list,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						$elm$json$Json$Encode$string(a),
						function ($) {
						return $elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'definitions',
									$elm$json$Json$Encode$list($elm$json$Json$Encode$string)($.definitions)),
									_Utils_Tuple2(
									'familiarityLevel',
									$elm$json$Json$Encode$int($.familiarityLevel)),
									_Utils_Tuple2(
									'notes',
									$elm$json$Json$Encode$string($.notes)),
									_Utils_Tuple2(
									'reviewHistory',
									$elm$json$Json$Encode$list(
										function ($) {
											return $elm$json$Json$Encode$object(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'lessonId',
														function ($) {
															return A3($elm$core$Maybe$destruct, $elm$json$Json$Encode$null, $elm$json$Json$Encode$string, $);
														}($.lessonId)),
														_Utils_Tuple2(
														'timestamp',
														$elm$json$Json$Encode$int($.timestamp))
													]));
										})($.reviewHistory)),
									_Utils_Tuple2(
									'romanization',
									$elm$json$Json$Encode$string($.romanization)),
									_Utils_Tuple2(
									'tags',
									$elm$json$Json$Encode$list($elm$json$Json$Encode$string)($.tags)),
									_Utils_Tuple2(
									'wordOrPhrase',
									$elm$json$Json$Encode$list($elm$json$Json$Encode$string)($.wordOrPhrase))
								]));
					}(b)
					]));
		}));
var $elm_community$maybe_extra$Maybe$Extra$unwrap = F3(
	function (_default, f, m) {
		if (m.$ === 'Nothing') {
			return _default;
		} else {
			var a = m.a;
			return f(a);
		}
	});
var $author$project$EnterNewWops$cleanUpWopKey = function (wop) {
	return _Utils_update(
		wop,
		{
			wordOrPhrase: A2(
				$elm$core$List$filter,
				A2($elm$core$Basics$composeL, $elm$core$Basics$not, $elm$core$String$isEmpty),
				wop.wordOrPhrase)
		});
};
var $author$project$EnterNewWops$setWopKey = F2(
	function (wop, key) {
		return _Utils_update(
			wop,
			{
				wordOrPhrase: A2(
					$elm$core$List$map,
					$elm$core$String$trim,
					A2($elm$core$String$split, ' ', key))
			});
	});
var $author$project$EnterNewWops$wopNotEmpty = function (wop) {
	return $elm$core$List$length(wop.wordOrPhrase) >= 1;
};
var $author$project$EnterNewWops$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'EditWOPKey':
				var key = msg.a;
				return _Utils_update(
					model,
					{
						newWop: A2($author$project$EnterNewWops$setWopKey, model.newWop, key)
					});
			case 'EditWOPDefinition':
				var def = msg.a;
				return _Utils_update(
					model,
					{
						newWop: A3($author$project$WordOrPhrase$setDefinition, 0, def, model.newWop)
					});
			case 'SaveNewWOP':
				return $author$project$EnterNewWops$wopNotEmpty(model.newWop) ? _Utils_update(
					model,
					{
						createdWops: A2(
							$elm$core$List$cons,
							$author$project$EnterNewWops$cleanUpWopKey(model.newWop),
							model.createdWops),
						newWop: $author$project$EnterNewWops$initWOP
					}) : model;
			default:
				return model;
		}
	});
var $author$project$WordOrPhrase$update = function (wopKey) {
	return $elm$core$Dict$update(
		$author$project$WordOrPhrase$removeTashkyl(wopKey));
};
var $author$project$EnterNewWops$viewPage = function (model) {
	return _Utils_update(
		model,
		{viewingEnterNewWops: true});
};
var $author$project$Main$update = F2(
	function (msg, model) {
		return A2(
			$elm$core$Tuple$mapFirst,
			function (updatedModel) {
				var updateLessonsDueForReview = function (um) {
					var wops = um.wops;
					var lessons = um.lessons;
					return ((!_Utils_eq(wops, model.wops)) || (!_Utils_eq(lessons, model.lessons))) ? _Utils_update(
						um,
						{
							lessonsDueForReview: $author$project$DueForReview$getLessonsDueForReview(um)
						}) : um;
				};
				return updateLessonsDueForReview(updatedModel);
			},
			function () {
				switch (msg.$) {
					case 'Tick':
						var posix = msg.a;
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{tick: posix}));
					case 'GotKalimniEvent':
						var kalimniEvent = msg.a;
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{
									lastKalimniEvent: $elm$core$Maybe$Just(kalimniEvent)
								}));
					case 'ToggleDrawInLesson':
						return $author$project$Utils$pure(
							model.drawInLesson ? _Utils_update(
								model,
								{drawInLesson: !model.drawInLesson}) : _Utils_update(
								model,
								{drawInLesson: !model.drawInLesson, startingRectangleCoordinate: $elm$core$Maybe$Nothing}));
					case 'LessonImageClick':
						if (model.drawInLesson) {
							var _v1 = model.startingRectangleCoordinate;
							if (_v1.$ === 'Just') {
								var topLeft = _v1.a;
								return $author$project$Utils$pure(
									A2(
										$elm$core$Maybe$withDefault,
										model,
										A2(
											$elm$core$Maybe$map,
											function (_v2) {
												var relativeX = _v2.relativeX;
												var relativeY = _v2.relativeY;
												return _Utils_update(
													model,
													{
														selectedKalimniLessonPage: A2(
															$elm$core$Debug$log,
															'blah',
															A2(
																$elm$core$Maybe$map,
																function (lessonPage) {
																	return _Utils_update(
																		lessonPage,
																		{
																			textBoxes: A2(
																				$elm$core$List$cons,
																				{
																					bottomRight: _Utils_Tuple2(relativeX, relativeY),
																					text: '',
																					topLeft: topLeft
																				},
																				lessonPage.textBoxes)
																		});
																},
																model.selectedKalimniLessonPage)),
														startingRectangleCoordinate: $elm$core$Maybe$Nothing
													});
											},
											model.lastKalimniEvent)));
							} else {
								return $author$project$Utils$pure(
									_Utils_update(
										model,
										{
											startingRectangleCoordinate: A2(
												$elm$core$Maybe$andThen,
												function (ke) {
													return $elm$core$Maybe$Just(
														_Utils_Tuple2(ke.relativeX, ke.relativeY));
												},
												model.lastKalimniEvent)
										}));
							}
						} else {
							return $author$project$Utils$pure(model);
						}
					case 'EnterNewWopsMsg':
						var childMsg = msg.a;
						if (childMsg.$ === 'GoBack') {
							var newWops = model.enterNewWops.createdWops;
							var mergedIntoWopDict = A3(
								$elm$core$List$foldl,
								F2(
									function (wop, dict) {
										return A3(
											$author$project$WordOrPhrase$insert,
											$author$project$WordOrPhrase$key(wop),
											wop,
											dict);
									}),
								model.wops,
								newWops);
							return $author$project$Utils$pure(
								_Utils_update(
									model,
									{enterNewWops: $author$project$EnterNewWops$init, wops: mergedIntoWopDict}));
						} else {
							return $author$project$Utils$pure(
								_Utils_update(
									model,
									{
										enterNewWops: A2($author$project$EnterNewWops$update, childMsg, model.enterNewWops)
									}));
						}
					case 'NavigateToEnterNewWops':
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{
									enterNewWops: $author$project$EnterNewWops$viewPage(model.enterNewWops)
								}));
					case 'KeyPress':
						var key = msg.a;
						return ((!$elm$core$String$isEmpty(model.selectedWop)) && _Utils_eq(model.selectedWop, model.hoveredWord)) ? A3(
							$elm_community$maybe_extra$Maybe$Extra$unwrap,
							$author$project$Utils$pure(model),
							$elm$core$Basics$identity,
							A2(
								$elm$core$Maybe$andThen,
								function (n) {
									return ((n >= 1) && (n <= 4)) ? $elm$core$Maybe$Just(
										A2(
											$author$project$Main$update,
											A2($author$project$Main$SetSelectedWOPFamiliarityLevel, model.selectedWop, n),
											model)) : $elm$core$Maybe$Nothing;
								},
								$elm$core$String$toInt(key))) : $author$project$Utils$pure(model);
					case 'WordHoverStart':
						var word = msg.a;
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{hoveredWord: word}));
					case 'WordHoverLeave':
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{hoveredWord: ''}));
					case 'SaveDataModelToClipboard':
						return _Utils_Tuple2(
							model,
							$author$project$Main$saveLocalStorageToClipboard(_Utils_Tuple0));
					case 'ChangeNewLessonText':
						var text = msg.a;
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{newLessonText: text}));
					case 'ChangeNewLessonTitle':
						var title = msg.a;
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{newLessonTitle: title}));
					case 'CreateNewLesson':
						return A2(
							$author$project$Utils$impure,
							_Utils_update(
								model,
								{
									lessons: A3(
										$elm$core$Dict$insert,
										model.newLessonTitle,
										$author$project$Main$makeLesson(model.newLessonText),
										model.lessons),
									newLessonText: '',
									newLessonTitle: ''
								}),
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.lessons;
								},
								A2($elm$core$Basics$composeR, $elm$core$Dict$toList, $author$project$Main$storeLessons)));
					case 'UpdateLesson':
						var _v4 = msg.a;
						var existingTitle = _v4.a;
						var newTitle = _v4.b;
						var processTitleChange = function (m) {
							return (!_Utils_eq(existingTitle, newTitle)) ? _Utils_update(
								m,
								{
									lessonTranslations: A2($elm$core$Dict$remove, existingTitle, m.lessonTranslations),
									lessons: A2($elm$core$Dict$remove, existingTitle, m.lessons),
									selectedLesson: newTitle
								}) : m;
						};
						var existingTranslation = A2($elm$core$Dict$get, existingTitle, model.lessonTranslations);
						var newModel = processTitleChange(
							_Utils_update(
								model,
								{
									lessonTranslations: A2(
										$elm$core$Maybe$withDefault,
										model.lessonTranslations,
										A2(
											$elm$core$Maybe$map,
											function (et) {
												return A3($elm$core$Dict$insert, newTitle, et, model.lessonTranslations);
											},
											existingTranslation)),
									lessons: function () {
										var _v5 = A2($elm$core$Dict$get, existingTitle, model.lessons);
										if (_v5.$ === 'Just') {
											var lesson = _v5.a;
											return A3(
												$elm$core$Dict$insert,
												newTitle,
												_Utils_update(
													lesson,
													{text: model.newLessonText}),
												model.lessons);
										} else {
											return model.lessons;
										}
									}()
								}));
						var updateAudioName = (!_Utils_eq(existingTitle, newTitle)) ? $elm$http$Http$post(
							{
								body: $elm$http$Http$jsonBody(
									$elm$json$Json$Encode$object(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'old',
												$elm$json$Json$Encode$string(existingTitle)),
												_Utils_Tuple2(
												'new',
												$elm$json$Json$Encode$string(newTitle)),
												_Utils_Tuple2(
												'type',
												$elm$json$Json$Encode$string(
													A2(
														$elm$core$Maybe$withDefault,
														'wav',
														A2(
															$elm$core$Maybe$map,
															function ($) {
																return $.audioFileType;
															},
															A2($elm$core$Dict$get, newTitle, newModel.lessons)))))
											]))),
								expect: $elm$http$Http$expectString($author$project$Main$BackendAudioUpdated),
								url: 'http://localhost:3000/updateAudioName'
							}) : $elm$core$Platform$Cmd$none;
						return _Utils_Tuple2(
							newModel,
							$elm$core$Platform$Cmd$batch(
								_List_fromArray(
									[
										$author$project$Main$storeLessons(
										$elm$core$Dict$toList(newModel.lessons)),
										$author$project$Main$storeLessonTranslations(
										$elm$core$Dict$toList(newModel.lessonTranslations)),
										updateAudioName
									])));
					case 'ChangeLessonAudioFileType':
						var newFileType = msg.a;
						return A2(
							$author$project$Utils$impure,
							_Utils_update(
								model,
								{
									lessons: A3(
										$elm$core$Dict$update,
										model.selectedLesson,
										$elm$core$Maybe$map(
											function (lesson) {
												return _Utils_update(
													lesson,
													{audioFileType: newFileType});
											}),
										model.lessons)
								}),
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.lessons;
								},
								A2($elm$core$Basics$composeR, $elm$core$Dict$toList, $author$project$Main$storeLessons)));
					case 'BackendAudioUpdated':
						return $author$project$Utils$pure(model);
					case 'SelectLesson':
						var title = msg.a;
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{
									newLessonText: A2(
										$elm$core$Maybe$withDefault,
										'',
										A2(
											$elm$core$Maybe$map,
											function ($) {
												return $.text;
											},
											A2($elm$core$Dict$get, title, model.lessons))),
									newLessonTitle: title,
									selectedLesson: title,
									selectedWop: ''
								}));
					case 'DeselectLesson':
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{newLessonText: '', newLessonTitle: '', selectedLesson: '', selectedWop: ''}));
					case 'SelectWOP':
						var lessonId = msg.a.lessonId;
						var wopKey = msg.a.wopKey;
						var timestamp = msg.b;
						return A2(
							$author$project$Utils$impure,
							_Utils_update(
								model,
								{
									selectedWop: wopKey,
									selectedWopTagsBuffer: '',
									wops: A3(
										$author$project$WordOrPhrase$update,
										wopKey,
										$elm$core$Maybe$map(
											A2($author$project$WordOrPhrase$addReviewTime, timestamp, lessonId)),
										model.wops)
								}),
							function (newModel) {
								return $elm$core$Platform$Cmd$batch(
									_List_fromArray(
										[
											$author$project$Main$storeWops(
											$elm$core$Dict$toList(newModel.wops)),
											function () {
											var _v6 = A2($author$project$WordOrPhrase$get, newModel.selectedWop, newModel.wops);
											if (_v6.$ === 'Just') {
												return $elm$core$Platform$Cmd$none;
											} else {
												return $author$project$Main$autofocusId('newWopDefinition');
											}
										}()
										]));
							});
					case 'DeselectWOP':
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{selectedWop: ''}));
					case 'EditSelectedWOPDefinition':
						var defNumber = msg.a;
						var definition = msg.b;
						return A2(
							$author$project$Utils$impure,
							_Utils_update(
								model,
								{
									wops: A3(
										$author$project$WordOrPhrase$update,
										model.selectedWop,
										$elm$core$Maybe$map(
											A2($author$project$WordOrPhrase$setDefinition, defNumber, definition)),
										model.wops)
								}),
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.wops;
								},
								A2($elm$core$Basics$composeR, $elm$core$Dict$toList, $author$project$Main$storeWops)));
					case 'EditSelectedWOPNotes':
						var notes = msg.a;
						return A2(
							$author$project$Utils$impure,
							_Utils_update(
								model,
								{
									wops: A3(
										$author$project$WordOrPhrase$update,
										model.selectedWop,
										$elm$core$Maybe$map(
											$author$project$WordOrPhrase$setNotes(notes)),
										model.wops)
								}),
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.wops;
								},
								A2($elm$core$Basics$composeR, $elm$core$Dict$toList, $author$project$Main$storeWops)));
					case 'SetSelectedWOPFamiliarityLevel':
						var selectedWop = msg.a;
						var familiarityLevel = msg.b;
						return A2(
							$author$project$Utils$impure,
							_Utils_update(
								model,
								{
									wops: A3(
										$author$project$WordOrPhrase$update,
										selectedWop,
										$elm$core$Maybe$andThen(
											$author$project$WordOrPhrase$setFamiliarityLevel(familiarityLevel)),
										model.wops)
								}),
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.wops;
								},
								A2($elm$core$Basics$composeR, $elm$core$Dict$toList, $author$project$Main$storeWops)));
					case 'EditSelectedWOPRomanization':
						var romanization = msg.a;
						return A2(
							$author$project$Utils$impure,
							_Utils_update(
								model,
								{
									wops: A3(
										$author$project$WordOrPhrase$update,
										model.selectedWop,
										$elm$core$Maybe$map(
											function (wop) {
												return _Utils_update(
													wop,
													{romanization: romanization});
											}),
										model.wops)
								}),
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.wops;
								},
								A2($elm$core$Basics$composeR, $elm$core$Dict$toList, $author$project$Main$storeWops)));
					case 'EditSelectedWOPTagsBuffer':
						var tagString = msg.a;
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{selectedWopTagsBuffer: tagString}));
					case 'SetSelectedWOPTags':
						var tagString = msg.a;
						return A2(
							$author$project$Utils$impure,
							_Utils_update(
								model,
								{
									selectedWopTagsBuffer: '',
									wops: A3(
										$author$project$WordOrPhrase$update,
										model.selectedWop,
										$elm$core$Maybe$map(
											$author$project$WordOrPhrase$setTags(tagString)),
										model.wops)
								}),
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.wops;
								},
								A2($elm$core$Basics$composeR, $elm$core$Dict$toList, $author$project$Main$storeWops)));
					case 'EditSelectedNewWOPDefinition':
						var definition = msg.a;
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{newWopDefinition: definition}));
					case 'SaveSelectedNewWOP':
						var wopKeyList = $author$project$WordOrPhrase$keyIsPhrase(model.selectedWop) ? A2($elm$core$String$split, ' ', model.selectedWop) : _List_fromArray(
							[model.selectedWop]);
						return A2(
							$author$project$Utils$impure,
							_Utils_update(
								model,
								{
									newWopDefinition: '',
									wops: A3(
										$author$project$WordOrPhrase$insert,
										model.selectedWop,
										A2($author$project$WordOrPhrase$makeWOP, wopKeyList, model.newWopDefinition),
										model.wops)
								}),
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.wops;
								},
								A2($elm$core$Basics$composeR, $elm$core$Dict$toList, $author$project$Main$storeWops)));
					case 'AddTranslationToSelectedLesson':
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{
									lessonTranslations: A3($elm$core$Dict$insert, model.selectedLesson, '', model.lessonTranslations)
								}));
					case 'EditTranslationOfSelectedLesson':
						var newTranslation = msg.a;
						return A2(
							$author$project$Utils$impure,
							_Utils_update(
								model,
								{
									lessonTranslations: A3($elm$core$Dict$insert, model.selectedLesson, newTranslation, model.lessonTranslations)
								}),
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.lessonTranslations;
								},
								A2($elm$core$Basics$composeR, $elm$core$Dict$toList, $author$project$Main$storeLessonTranslations)));
					case 'MouseDownOnWord':
						var lineIndex = msg.a;
						var wordIndex = msg.b;
						var word = msg.c;
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{
									mouseDownWord: _Utils_Tuple3(lineIndex, wordIndex, word)
								}));
					case 'OpenPhraseCreationUI':
						var lineIndexEnd = msg.a;
						var wordIndexEnd = msg.b;
						var _v7 = model.mouseDownWord;
						var lineIndexStart = _v7.a;
						var wordIndexStart = _v7.b;
						if ((!_Utils_eq(wordIndexEnd, wordIndexStart)) && _Utils_eq(lineIndexEnd, lineIndexStart)) {
							var selectedLessonText = A2(
								$elm$core$Maybe$withDefault,
								'',
								A2(
									$elm$core$Maybe$map,
									function ($) {
										return $.text;
									},
									A2($elm$core$Dict$get, model.selectedLesson, model.lessons)));
							var line = A2(
								$elm$core$Maybe$withDefault,
								'',
								A2(
									$elm_community$list_extra$List$Extra$getAt,
									lineIndexEnd,
									$author$project$Main$splitIntoCleanLines(selectedLessonText)));
							var wordsAndNonWordsInPhraseSegment = function () {
								var smaller = A2($elm$core$Basics$min, wordIndexStart, wordIndexEnd);
								var larger = A2($elm$core$Basics$max, wordIndexStart, wordIndexEnd);
								return A2(
									$elm$core$List$filter,
									function (wordOrNonWord) {
										return !_Utils_eq(
											wordOrNonWord,
											$author$project$WordDisplay$DisplayNonWord(' '));
									},
									A2(
										$elm$core$List$take,
										(larger - smaller) + 1,
										A2(
											$elm$core$List$drop,
											smaller,
											$author$project$WordDisplay$markWordCharsFromNonWordChars(line))));
							}();
							var canConstructPhrase = A2(
								$elm$core$List$all,
								function (wordOrNonWord) {
									if (wordOrNonWord.$ === 'DisplayNonWord') {
										return false;
									} else {
										return true;
									}
								},
								wordsAndNonWordsInPhraseSegment);
							return canConstructPhrase ? A2(
								$author$project$Main$update,
								$author$project$Main$GetCurrentTimeAndThen(
									$author$project$Main$SelectWOP(
										{
											lessonId: model.selectedLesson,
											wopKey: A2(
												$elm$core$String$join,
												' ',
												A2(
													$elm$core$List$map,
													function (word) {
														if (word.$ === 'DisplayWord') {
															var w = word.a;
															return w;
														} else {
															return '';
														}
													},
													wordsAndNonWordsInPhraseSegment))
										})),
								model) : $author$project$Utils$pure(model);
						} else {
							return $author$project$Utils$pure(model);
						}
					case 'MarkLessonAsReviewed':
						var timestamp = msg.a;
						var lessonWops = A2(
							$author$project$Lesson$getWops,
							model.wops,
							A2(
								$elm$core$Maybe$withDefault,
								'',
								A2(
									$elm$core$Maybe$map,
									function ($) {
										return $.text;
									},
									A2($elm$core$Dict$get, model.selectedLesson, model.lessons))));
						var updatedWopReviews = A3(
							$elm$core$List$foldl,
							function (wop) {
								return A2(
									$elm$core$Dict$update,
									$author$project$WordOrPhrase$key(wop),
									$elm$core$Maybe$map(
										A2($author$project$WordOrPhrase$addReviewTime, timestamp, model.selectedLesson)));
							},
							model.wops,
							lessonWops);
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{wops: updatedWopReviews}));
					case 'NavigateToFlashcardPage':
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{onFlashcardPage: true}));
					case 'MakeNewWopFlashcardSet':
						var maybeRandomizedWops = msg.a;
						var randomizedWopsGenerator = msg.b;
						if (maybeRandomizedWops.$ === 'Just') {
							var wops = maybeRandomizedWops.a;
							return A2(
								$author$project$Utils$impure,
								_Utils_update(
									model,
									{
										newWopFlashcards: $wernerdegroot$listzipper$List$Zipper$fromList(
											A2($elm$core$List$take, 100, wops))
									}),
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.newWopFlashcards;
									},
									A2(
										$elm$core$Basics$composeR,
										$elm$core$Maybe$map($author$project$Utils$deconstructZipper),
										$author$project$Main$storeNewWopFlashcards)));
						} else {
							return _Utils_Tuple2(
								model,
								A2(
									$elm$random$Random$generate,
									function (wops) {
										return A2(
											$author$project$Main$MakeNewWopFlashcardSet,
											$elm$core$Maybe$Just(wops),
											randomizedWopsGenerator);
									},
									randomizedWopsGenerator));
						}
					case 'NextFlashcard':
						return A2(
							$author$project$Utils$impure,
							_Utils_update(
								model,
								{
									newWopFlashcards: A2($elm$core$Maybe$andThen, $wernerdegroot$listzipper$List$Zipper$next, model.newWopFlashcards)
								}),
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.newWopFlashcards;
								},
								A2(
									$elm$core$Basics$composeR,
									$elm$core$Maybe$map($author$project$Utils$deconstructZipper),
									$author$project$Main$storeNewWopFlashcards)));
					case 'PrepareForLessonWithFlashcards':
						var lessonText = msg.a;
						var getSentencesForWop = $author$project$Main$filterSentencesContainingWop(
							$author$project$Main$extractSentences(lessonText));
						var wopsWithSentence = A2(
							$elm$core$List$indexedMap,
							F2(
								function (i, wop) {
									var sentences = getSentencesForWop(wop);
									var chosenSentence = A2(
										$elm$core$Maybe$withDefault,
										'BAD SENTENCE',
										A2(
											$elm_community$list_extra$List$Extra$getAt,
											i % $elm$core$List$length(sentences),
											sentences));
									return _Utils_Tuple2(wop, chosenSentence);
								}),
							$elm$core$List$reverse(
								A2(
									$elm$core$List$filter,
									function (_v13) {
										var familiarityLevel = _v13.familiarityLevel;
										return familiarityLevel <= 2;
									},
									A2($author$project$Lesson$getWops, model.wops, lessonText))));
						var flashcards = $author$project$Flashcard$makeflashcards(wopsWithSentence);
						var _v11 = $author$project$Main$extractSentences(lessonText);
						var _v12 = A2(
							$elm$core$List$map,
							function (word) {
								return A2(
									$author$project$WordOrPhrase$makeWOP,
									_List_fromArray(
										[word]),
									'');
							},
							A2($author$project$Lesson$getUnidentifiedWordsInLesson, lessonText, model.wops));
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{
									lessonFlashcards: $elm$core$Maybe$Just(flashcards)
								}));
					case 'FlipLessonFlashcard':
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{flashcardFlipped: true}));
					case 'NextLessonFlashcard':
						var flashcard = msg.a;
						var remembered = msg.b;
						var timestamp = msg.c;
						var updatedFlashcard = A3($author$project$Flashcard$addHistoryEntry, timestamp, remembered, flashcard);
						var moveToEnd = _Utils_ap(
							A2(
								$elm$core$List$filter,
								$elm$core$Basics$neq(flashcard),
								A2($elm$core$Maybe$withDefault, _List_Nil, model.lessonFlashcards)),
							_List_fromArray(
								[updatedFlashcard]));
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{
									flashcardFlipped: false,
									lessonFlashcards: $elm$core$Maybe$Just(moveToEnd)
								}));
					case 'FinishFlashcardSession':
						return $author$project$Utils$pure(
							_Utils_update(
								model,
								{lessonFlashcards: $elm$core$Maybe$Nothing}));
					default:
						var toMsg = msg.a;
						return _Utils_Tuple2(
							model,
							A2($elm$core$Task$perform, toMsg, $elm$time$Time$now));
				}
			}());
	});
var $author$project$Main$EnterNewWopsMsg = function (a) {
	return {$: 'EnterNewWopsMsg', a: a};
};
var $author$project$Main$NavigateToEnterNewWops = {$: 'NavigateToEnterNewWops'};
var $author$project$Main$NavigateToFlashcardPage = {$: 'NavigateToFlashcardPage'};
var $author$project$Main$SaveDataModelToClipboard = {$: 'SaveDataModelToClipboard'};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$div = _VirtualDom_node('div');
var $author$project$Main$NextFlashcard = {$: 'NextFlashcard'};
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$regex$Regex$contains = _Regex_contains;
var $author$project$WordOrPhrase$displayFamiliarityLevel = function (familiarityLevel) {
	switch (familiarityLevel) {
		case 1:
			return 'New';
		case 2:
			return 'Recognized';
		case 3:
			return 'Familiar';
		case 4:
			return 'Learned';
		default:
			return '';
	}
};
var $elm_community$list_extra$List$Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return $elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$findIndexHelp = F3(
	function (index, predicate, list) {
		findIndexHelp:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var x = list.a;
				var xs = list.b;
				if (predicate(x)) {
					return $elm$core$Maybe$Just(index);
				} else {
					var $temp$index = index + 1,
						$temp$predicate = predicate,
						$temp$list = xs;
					index = $temp$index;
					predicate = $temp$predicate;
					list = $temp$list;
					continue findIndexHelp;
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$findIndex = $elm_community$list_extra$List$Extra$findIndexHelp(0);
var $author$project$WordOrPhrase$getFamiliarityLevel = F2(
	function (wopKey, wops) {
		return A2(
			$elm$core$Maybe$withDefault,
			0,
			A2(
				$elm$core$Maybe$map,
				function (wop) {
					return wop.familiarityLevel;
				},
				A2($author$project$WordOrPhrase$get, wopKey, wops)));
	});
var $author$project$WordOrPhrase$isPhrase = function (_v0) {
	var wordOrPhrase = _v0.wordOrPhrase;
	return $elm$core$List$length(wordOrPhrase) > 1;
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm_community$list_extra$List$Extra$splitAt = F2(
	function (n, xs) {
		return _Utils_Tuple2(
			A2($elm$core$List$take, n, xs),
			A2($elm$core$List$drop, n, xs));
	});
var $elm_community$list_extra$List$Extra$splitWhen = F2(
	function (predicate, list) {
		return A2(
			$elm$core$Maybe$map,
			function (i) {
				return A2($elm_community$list_extra$List$Extra$splitAt, i, list);
			},
			A2($elm_community$list_extra$List$Extra$findIndex, predicate, list));
	});
var $elm_community$list_extra$List$Extra$takeWhileRight = function (p) {
	var step = F2(
		function (x, _v0) {
			var xs = _v0.a;
			var free = _v0.b;
			return (p(x) && free) ? _Utils_Tuple2(
				A2($elm$core$List$cons, x, xs),
				true) : _Utils_Tuple2(xs, false);
		});
	return A2(
		$elm$core$Basics$composeL,
		$elm$core$Tuple$first,
		A2(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2(_List_Nil, true)));
};
var $author$project$Main$terminalPunctuationRx = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(?=[.???!\\n]+)'));
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$core$String$toLower = _String_toLower;
var $elm_community$list_extra$List$Extra$uncons = function (list) {
	if (!list.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var first = list.a;
		var rest = list.b;
		return $elm$core$Maybe$Just(
			_Utils_Tuple2(first, rest));
	}
};
var $author$project$Main$getExampleSentenceForWop = F3(
	function (wop, lessons, wops) {
		if ($author$project$WordOrPhrase$isPhrase(wop)) {
			return A2(
				$elm$html$Html$p,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('phrases currently unsupported')
					]));
		} else {
			var word = A2($elm$core$String$join, ' ', wop.wordOrPhrase);
			var wordFamiliarityClass = $elm$html$Html$Attributes$class(
				$elm$core$String$toLower(
					$author$project$WordOrPhrase$displayFamiliarityLevel(
						A2($author$project$WordOrPhrase$getFamiliarityLevel, word, wops))));
			var lessonContainingWord = A2(
				$elm_community$list_extra$List$Extra$find,
				function (lesson) {
					return A2(
						$elm$core$List$any,
						function (displayWord) {
							if (displayWord.$ === 'DisplayWord') {
								var lessonWord = displayWord.a;
								return A2($author$project$WordOrPhrase$tashkylEquivalent, word, lessonWord);
							} else {
								return false;
							}
						},
						$author$project$WordDisplay$markWordCharsFromNonWordChars(lesson));
				},
				lessons);
			var fullSentenceContainingWord = A2(
				$elm$core$Maybe$map,
				function (lesson) {
					var isSentenceTerminal = function (displayWord) {
						if (displayWord.$ === 'DisplayNonWord') {
							var nonWord = displayWord.a;
							return A2($elm$regex$Regex$contains, $author$project$Main$terminalPunctuationRx, nonWord);
						} else {
							return true;
						}
					};
					var _v2 = A2(
						$elm$core$Maybe$withDefault,
						_Utils_Tuple2(_List_Nil, _List_Nil),
						A2(
							$elm_community$list_extra$List$Extra$splitWhen,
							function (displayWord) {
								if (displayWord.$ === 'DisplayWord') {
									var lessonWord = displayWord.a;
									return !A2($author$project$WordOrPhrase$tashkylEquivalent, word, lessonWord);
								} else {
									return true;
								}
							},
							$author$project$WordDisplay$markWordCharsFromNonWordChars(lesson)));
					var beforeMatch = _v2.a;
					var withMatch = _v2.b;
					var sentenceBeforeMatch = A2(
						$elm_community$list_extra$List$Extra$takeWhileRight,
						A2($elm$core$Basics$composeL, $elm$core$Basics$not, isSentenceTerminal),
						beforeMatch);
					var _v4 = A2(
						$elm$core$Maybe$withDefault,
						_Utils_Tuple2(
							$author$project$WordDisplay$DisplayWord(''),
							_List_Nil),
						$elm_community$list_extra$List$Extra$uncons(withMatch));
					var match = _v4.a;
					var afterMatch = _v4.b;
					var sentenceAfterMatch = function (endIndex) {
						return A2($elm$core$List$take, endIndex + 1, afterMatch);
					}(
						A2(
							$elm$core$Maybe$withDefault,
							0,
							A2($elm_community$list_extra$List$Extra$findIndex, isSentenceTerminal, afterMatch)));
					return $elm$core$List$concat(
						_List_fromArray(
							[
								sentenceBeforeMatch,
								_List_fromArray(
								[match]),
								sentenceAfterMatch
							]));
				},
				lessonContainingWord);
			if (fullSentenceContainingWord.$ === 'Nothing') {
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(word)
						]));
			} else {
				var exampleSentence = fullSentenceContainingWord.a;
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('example-sentence')
						]),
					A2(
						$elm$core$List$map,
						function (displayWordInSentence) {
							switch (displayWordInSentence.$) {
								case 'DisplayWord':
									var wordInSentence = displayWordInSentence.a;
									return A2($author$project$WordOrPhrase$tashkylEquivalent, wordInSentence, word) ? A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('target-word-in-sentence'),
												wordFamiliarityClass
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(word)
											])) : A2(
										$elm$html$Html$span,
										_List_fromArray(
											[wordFamiliarityClass]),
										_List_fromArray(
											[
												$elm$html$Html$text(wordInSentence)
											]));
								case 'DisplayNonWord':
									var nonWordInSentence = displayWordInSentence.a;
									return A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('non-word')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(nonWordInSentence)
											]));
								default:
									return A2(
										$elm$html$Html$div,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('INVALID')
											]));
							}
						},
						exampleSentence));
			}
		}
	});
var $author$project$WordOrPhrase$listWopsOfLevel = function (familiarityLevel) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$Dict$values,
		$elm$core$List$filter(
			function (wop) {
				return _Utils_eq(wop.familiarityLevel, familiarityLevel);
			}));
};
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
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
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm$random$Random$maxInt = 2147483647;
var $elm$random$Random$minInt = -2147483648;
var $elm_community$random_extra$Random$List$anyInt = A2($elm$random$Random$int, $elm$random$Random$minInt, $elm$random$Random$maxInt);
var $elm$random$Random$map3 = F4(
	function (func, _v0, _v1, _v2) {
		var genA = _v0.a;
		var genB = _v1.a;
		var genC = _v2.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v3 = genA(seed0);
				var a = _v3.a;
				var seed1 = _v3.b;
				var _v4 = genB(seed1);
				var b = _v4.a;
				var seed2 = _v4.b;
				var _v5 = genC(seed2);
				var c = _v5.a;
				var seed3 = _v5.b;
				return _Utils_Tuple2(
					A3(func, a, b, c),
					seed3);
			});
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $elm$random$Random$independentSeed = $elm$random$Random$Generator(
	function (seed0) {
		var makeIndependentSeed = F3(
			function (state, b, c) {
				return $elm$random$Random$next(
					A2($elm$random$Random$Seed, state, (1 | (b ^ c)) >>> 0));
			});
		var gen = A2($elm$random$Random$int, 0, 4294967295);
		return A2(
			$elm$random$Random$step,
			A4($elm$random$Random$map3, makeIndependentSeed, gen, gen, gen),
			seed0);
	});
var $elm_community$random_extra$Random$List$shuffle = function (list) {
	return A2(
		$elm$random$Random$map,
		function (independentSeed) {
			return A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2(
					$elm$core$List$sortBy,
					$elm$core$Tuple$second,
					A3(
						$elm$core$List$foldl,
						F2(
							function (item, _v0) {
								var acc = _v0.a;
								var seed = _v0.b;
								var _v1 = A2($elm$random$Random$step, $elm_community$random_extra$Random$List$anyInt, seed);
								var tag = _v1.a;
								var nextSeed = _v1.b;
								return _Utils_Tuple2(
									A2(
										$elm$core$List$cons,
										_Utils_Tuple2(item, tag),
										acc),
									nextSeed);
							}),
						_Utils_Tuple2(_List_Nil, independentSeed),
						list).a));
		},
		$elm$random$Random$independentSeed);
};
var $wernerdegroot$listzipper$List$Zipper$toList = function (z) {
	return _Utils_ap(
		$wernerdegroot$listzipper$List$Zipper$before(z),
		_Utils_ap(
			_List_fromArray(
				[
					$wernerdegroot$listzipper$List$Zipper$current(z)
				]),
			$wernerdegroot$listzipper$List$Zipper$after(z)));
};
var $author$project$Main$flashcardView = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('flashcard-page')
			]),
		_List_fromArray(
			[
				function () {
				var _v0 = model.newWopFlashcards;
				if (_v0.$ === 'Just') {
					var newWopFlashcards = _v0.a;
					var wop = $wernerdegroot$listzipper$List$Zipper$current(newWopFlashcards);
					var totalSize = $elm$core$String$fromInt(
						$elm$core$List$length(
							$wernerdegroot$listzipper$List$Zipper$toList(newWopFlashcards)));
					var currentFlashcardNumber = $elm$core$String$fromInt(
						1 + $elm$core$List$length(
							$wernerdegroot$listzipper$List$Zipper$before(newWopFlashcards)));
					return A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$p,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('word new')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										A2($elm$core$String$join, ', ', wop.wordOrPhrase))
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										A2(
											$elm$core$Maybe$withDefault,
											'no definition',
											$elm$core$List$head(wop.definitions)))
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$Main$NextFlashcard)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Next Card')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('on flashcard ' + (currentFlashcardNumber + ('/' + totalSize)))
									])),
								A3(
								$author$project$Main$getExampleSentenceForWop,
								wop,
								A2(
									$elm$core$List$map,
									function ($) {
										return $.text;
									},
									$elm$core$Dict$values(model.lessons)),
								model.wops)
							]));
				} else {
					return A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								A2(
									$author$project$Main$MakeNewWopFlashcardSet,
									$elm$core$Maybe$Nothing,
									$elm_community$random_extra$Random$List$shuffle(
										A2($author$project$WordOrPhrase$listWopsOfLevel, 1, model.wops))))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Start a new set of new wops')
							]));
				}
			}()
			]));
};
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $author$project$Main$SelectLesson = function (a) {
	return {$: 'SelectLesson', a: a};
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$html$Html$h5 = _VirtualDom_node('h5');
var $elm_community$maybe_extra$Maybe$Extra$isJust = function (m) {
	if (m.$ === 'Nothing') {
		return false;
	} else {
		return true;
	}
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $myrho$elm_round$Round$addSign = F2(
	function (signed, str) {
		var isNotZero = A2(
			$elm$core$List$any,
			function (c) {
				return (!_Utils_eq(
					c,
					_Utils_chr('0'))) && (!_Utils_eq(
					c,
					_Utils_chr('.')));
			},
			$elm$core$String$toList(str));
		return _Utils_ap(
			(signed && isNotZero) ? '-' : '',
			str);
	});
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$core$String$cons = _String_cons;
var $elm$core$Char$fromCode = _Char_fromCode;
var $myrho$elm_round$Round$increaseNum = function (_v0) {
	var head = _v0.a;
	var tail = _v0.b;
	if (_Utils_eq(
		head,
		_Utils_chr('9'))) {
		var _v1 = $elm$core$String$uncons(tail);
		if (_v1.$ === 'Nothing') {
			return '01';
		} else {
			var headtail = _v1.a;
			return A2(
				$elm$core$String$cons,
				_Utils_chr('0'),
				$myrho$elm_round$Round$increaseNum(headtail));
		}
	} else {
		var c = $elm$core$Char$toCode(head);
		return ((c >= 48) && (c < 57)) ? A2(
			$elm$core$String$cons,
			$elm$core$Char$fromCode(c + 1),
			tail) : '0';
	}
};
var $elm$core$Basics$isInfinite = _Basics_isInfinite;
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padRight = F3(
	function (n, _char, string) {
		return _Utils_ap(
			string,
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)));
	});
var $elm$core$String$reverse = _String_reverse;
var $myrho$elm_round$Round$splitComma = function (str) {
	var _v0 = A2($elm$core$String$split, '.', str);
	if (_v0.b) {
		if (_v0.b.b) {
			var before = _v0.a;
			var _v1 = _v0.b;
			var after = _v1.a;
			return _Utils_Tuple2(before, after);
		} else {
			var before = _v0.a;
			return _Utils_Tuple2(before, '0');
		}
	} else {
		return _Utils_Tuple2('0', '0');
	}
};
var $myrho$elm_round$Round$toDecimal = function (fl) {
	var _v0 = A2(
		$elm$core$String$split,
		'e',
		$elm$core$String$fromFloat(
			$elm$core$Basics$abs(fl)));
	if (_v0.b) {
		if (_v0.b.b) {
			var num = _v0.a;
			var _v1 = _v0.b;
			var exp = _v1.a;
			var e = A2(
				$elm$core$Maybe$withDefault,
				0,
				$elm$core$String$toInt(
					A2($elm$core$String$startsWith, '+', exp) ? A2($elm$core$String$dropLeft, 1, exp) : exp));
			var _v2 = $myrho$elm_round$Round$splitComma(num);
			var before = _v2.a;
			var after = _v2.b;
			var total = _Utils_ap(before, after);
			var zeroed = (e < 0) ? A2(
				$elm$core$Maybe$withDefault,
				'0',
				A2(
					$elm$core$Maybe$map,
					function (_v3) {
						var a = _v3.a;
						var b = _v3.b;
						return a + ('.' + b);
					},
					A2(
						$elm$core$Maybe$map,
						$elm$core$Tuple$mapFirst($elm$core$String$fromChar),
						$elm$core$String$uncons(
							_Utils_ap(
								A2(
									$elm$core$String$repeat,
									$elm$core$Basics$abs(e),
									'0'),
								total))))) : A3(
				$elm$core$String$padRight,
				e + 1,
				_Utils_chr('0'),
				total);
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				zeroed);
		} else {
			var num = _v0.a;
			return _Utils_ap(
				(fl < 0) ? '-' : '',
				num);
		}
	} else {
		return '';
	}
};
var $myrho$elm_round$Round$roundFun = F3(
	function (functor, s, fl) {
		if ($elm$core$Basics$isInfinite(fl) || $elm$core$Basics$isNaN(fl)) {
			return $elm$core$String$fromFloat(fl);
		} else {
			var signed = fl < 0;
			var _v0 = $myrho$elm_round$Round$splitComma(
				$myrho$elm_round$Round$toDecimal(
					$elm$core$Basics$abs(fl)));
			var before = _v0.a;
			var after = _v0.b;
			var r = $elm$core$String$length(before) + s;
			var normalized = _Utils_ap(
				A2($elm$core$String$repeat, (-r) + 1, '0'),
				A3(
					$elm$core$String$padRight,
					r,
					_Utils_chr('0'),
					_Utils_ap(before, after)));
			var totalLen = $elm$core$String$length(normalized);
			var roundDigitIndex = A2($elm$core$Basics$max, 1, r);
			var increase = A2(
				functor,
				signed,
				A3($elm$core$String$slice, roundDigitIndex, totalLen, normalized));
			var remains = A3($elm$core$String$slice, 0, roundDigitIndex, normalized);
			var num = increase ? $elm$core$String$reverse(
				A2(
					$elm$core$Maybe$withDefault,
					'1',
					A2(
						$elm$core$Maybe$map,
						$myrho$elm_round$Round$increaseNum,
						$elm$core$String$uncons(
							$elm$core$String$reverse(remains))))) : remains;
			var numLen = $elm$core$String$length(num);
			var numZeroed = (num === '0') ? num : ((s <= 0) ? _Utils_ap(
				num,
				A2(
					$elm$core$String$repeat,
					$elm$core$Basics$abs(s),
					'0')) : ((_Utils_cmp(
				s,
				$elm$core$String$length(after)) < 0) ? (A3($elm$core$String$slice, 0, numLen - s, num) + ('.' + A3($elm$core$String$slice, numLen - s, numLen, num))) : _Utils_ap(
				before + '.',
				A3(
					$elm$core$String$padRight,
					s,
					_Utils_chr('0'),
					after))));
			return A2($myrho$elm_round$Round$addSign, signed, numZeroed);
		}
	});
var $myrho$elm_round$Round$round = $myrho$elm_round$Round$roundFun(
	F2(
		function (signed, str) {
			var _v0 = $elm$core$String$uncons(str);
			if (_v0.$ === 'Nothing') {
				return false;
			} else {
				if ('5' === _v0.a.a.valueOf()) {
					if (_v0.a.b === '') {
						var _v1 = _v0.a;
						return !signed;
					} else {
						var _v2 = _v0.a;
						return true;
					}
				} else {
					var _v3 = _v0.a;
					var _int = _v3.a;
					return function (i) {
						return ((i > 53) && signed) || ((i >= 53) && (!signed));
					}(
						$elm$core$Char$toCode(_int));
				}
			}
		}));
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $elm$html$Html$Attributes$title = $elm$html$Html$Attributes$stringProperty('title');
var $author$project$Main$lessonsView = function (model) {
	var wordsOfLevel = function (n) {
		return $elm$core$String$fromInt(
			$elm$core$List$length(
				A2($author$project$WordOrPhrase$listWopsOfLevel, n, model.wops))) + (' ' + $author$project$WordOrPhrase$displayFamiliarityLevel(n));
	};
	var viewingFlashcards = $elm_community$maybe_extra$Maybe$Extra$isJust(model.lessonFlashcards);
	var buttonTitle = viewingFlashcards ? 'finish or cancel flashcards to change lesson' : '';
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('lessons-view')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h3,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('select a lesson')
					])),
				A2(
				$elm$html$Html$h5,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						'currently learning ' + ($elm$core$String$fromInt(
							$elm$core$Dict$size(model.wops)) + (' words!' + (' including: ' + A2(
							$elm$core$String$join,
							', ',
							A2(
								$elm$core$List$map,
								wordsOfLevel,
								_List_fromArray(
									[1, 2, 3, 4])))))))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('due-wops')
					]),
				_List_Nil),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('lesson-selector')
					]),
				A2(
					$elm$core$List$map,
					function (title) {
						return A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Events$onClick(
									$author$project$Main$SelectLesson(title)),
									$elm$html$Html$Attributes$disabled(viewingFlashcards),
									$elm$html$Html$Attributes$title(buttonTitle)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(title)
								]));
					},
					$elm$core$Dict$keys(model.lessons))),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('lessons-for-review')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('here\'s the top ten lessons to review:')
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('lesson-selector')
							]),
						A2(
							$elm$core$List$map,
							function (_v0) {
								var lessonTitle = _v0.lessonTitle;
								var densityRatio = _v0.densityRatio;
								var totalDueInLesson = _v0.totalDueInLesson;
								return A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Events$onClick(
											$author$project$Main$SelectLesson(lessonTitle)),
											$elm$html$Html$Attributes$disabled(viewingFlashcards),
											$elm$html$Html$Attributes$title(buttonTitle)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(lessonTitle),
											$elm$html$Html$text(
											' || ratio: ' + A2($myrho$elm_round$Round$round, 2, densityRatio)),
											$elm$html$Html$text(
											' || due wops: ' + $elm$core$String$fromInt(totalDueInLesson))
										]));
							},
							A2($elm$core$List$take, 10, model.lessonsDueForReview)))
					]))
			]));
};
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $author$project$Main$ChangeLessonAudioFileType = function (a) {
	return {$: 'ChangeLessonAudioFileType', a: a};
};
var $author$project$Main$ChangeNewLessonText = function (a) {
	return {$: 'ChangeNewLessonText', a: a};
};
var $author$project$Main$ChangeNewLessonTitle = function (a) {
	return {$: 'ChangeNewLessonTitle', a: a};
};
var $author$project$Main$CreateNewLesson = {$: 'CreateNewLesson'};
var $author$project$Main$DeselectLesson = {$: 'DeselectLesson'};
var $author$project$Main$UpdateLesson = function (a) {
	return {$: 'UpdateLesson', a: a};
};
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$html$Html$Attributes$cols = function (n) {
	return A2(
		_VirtualDom_attribute,
		'cols',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$Attributes$for = $elm$html$Html$Attributes$stringProperty('htmlFor');
var $elm$html$Html$form = _VirtualDom_node('form');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$html$Html$Attributes$name = $elm$html$Html$Attributes$stringProperty('name');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
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
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Main$newAndEditLessonView = function (model) {
	var lessonFileType = A2(
		$elm$core$Maybe$withDefault,
		'',
		A2(
			$elm$core$Maybe$map,
			function ($) {
				return $.audioFileType;
			},
			A2($elm$core$Dict$get, model.selectedLesson, model.lessons)));
	var buttonDisabled = $elm$core$String$isEmpty(model.selectedLesson) ? ((model.newLessonText === '') || ((model.newLessonTitle === '') || (!_Utils_eq(
		A2($elm$core$Dict$get, model.newLessonTitle, model.lessons),
		$elm$core$Maybe$Nothing)))) : (_Utils_eq(
		A2(
			$elm$core$Maybe$map,
			function ($) {
				return $.text;
			},
			A2($elm$core$Dict$get, model.selectedLesson, model.lessons)),
		$elm$core$Maybe$Just(model.newLessonText)) && (_Utils_eq(model.selectedLesson, model.newLessonTitle) || (!_Utils_eq(
		A2($elm$core$Dict$get, model.newLessonTitle, model.lessons),
		$elm$core$Maybe$Nothing))));
	var audioFileTypeRadioButtons = $elm$core$String$isEmpty(model.selectedLesson) ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('select file type for associated audio:')
					])),
				A2(
				$elm$html$Html$form,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'flex'),
						A2($elm$html$Html$Attributes$style, 'width', '120px'),
						A2($elm$html$Html$Attributes$style, 'margin-left', 'auto')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('radio'),
								$elm$html$Html$Attributes$name('fileTypeWav'),
								$elm$html$Html$Events$onInput($author$project$Main$ChangeLessonAudioFileType),
								$elm$html$Html$Attributes$checked(lessonFileType === 'wav'),
								$elm$html$Html$Attributes$value('wav')
							]),
						_List_Nil),
						A2(
						$elm$html$Html$label,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$for('fileTypeWav')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('wav')
							])),
						A2(
						$elm$html$Html$input,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$type_('radio'),
								$elm$html$Html$Attributes$name('fileTypeMp3'),
								$elm$html$Html$Events$onInput($author$project$Main$ChangeLessonAudioFileType),
								$elm$html$Html$Attributes$checked(lessonFileType === 'mp3'),
								$elm$html$Html$Attributes$value('mp3')
							]),
						_List_Nil),
						A2(
						$elm$html$Html$label,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$for('fileTypeMp3')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('mp3')
							]))
					]))
			]));
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('new-lesson-view')
			]),
		_List_fromArray(
			[
				$elm$core$String$isEmpty(model.selectedLesson) ? A2(
				$elm$html$Html$label,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Make a new lesson')
					])) : A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$label,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'display', 'block')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Edit lesson')
							])),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$DeselectLesson)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Deselect Lesson')
							]))
					])),
				A2(
				$elm$html$Html$input,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$placeholder('title'),
						$elm$html$Html$Attributes$value(model.newLessonTitle),
						$elm$html$Html$Events$onInput($author$project$Main$ChangeNewLessonTitle)
					]),
				_List_Nil),
				audioFileTypeRadioButtons,
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('textarea-container')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$rows(15),
								$elm$html$Html$Attributes$cols(60),
								$elm$html$Html$Events$onInput($author$project$Main$ChangeNewLessonText),
								$elm$html$Html$Attributes$value(model.newLessonText)
							]),
						_List_Nil)
					])),
				$elm$core$String$isEmpty(model.selectedLesson) ? A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Main$CreateNewLesson),
						$elm$html$Html$Attributes$disabled(buttonDisabled)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Create Lesson')
					])) : A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick(
						$author$project$Main$UpdateLesson(
							_Utils_Tuple2(model.selectedLesson, model.newLessonTitle))),
						$elm$html$Html$Attributes$disabled(buttonDisabled)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Update Lesson')
					]))
			]));
};
var $author$project$Main$MarkLessonAsReviewed = function (a) {
	return {$: 'MarkLessonAsReviewed', a: a};
};
var $author$project$Main$PrepareForLessonWithFlashcards = function (a) {
	return {$: 'PrepareForLessonWithFlashcards', a: a};
};
var $elm$html$Html$audio = _VirtualDom_node('audio');
var $elm$html$Html$Attributes$controls = $elm$html$Html$Attributes$boolProperty('controls');
var $author$project$Main$DeselectWOP = {$: 'DeselectWOP'};
var $author$project$Main$MouseDownOnWord = F3(
	function (a, b, c) {
		return {$: 'MouseDownOnWord', a: a, b: b, c: c};
	});
var $author$project$Main$OpenPhraseCreationUI = F3(
	function (a, b, c) {
		return {$: 'OpenPhraseCreationUI', a: a, b: b, c: c};
	});
var $author$project$Main$WordHoverLeave = {$: 'WordHoverLeave'};
var $author$project$Main$WordHoverStart = function (a) {
	return {$: 'WordHoverStart', a: a};
};
var $author$project$WordOrPhrase$allPhrases = function (dict) {
	return A2(
		$elm$core$List$filterMap,
		function (k) {
			var length = $elm$core$List$length(
				A2($elm$core$String$split, ' ', k));
			return (length === 1) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
				_Utils_Tuple2(k, length));
		},
		$elm$core$Dict$keys(dict));
};
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $author$project$WordDisplay$DisplayWordOfPhrase = function (a) {
	return {$: 'DisplayWordOfPhrase', a: a};
};
var $elm_community$list_extra$List$Extra$groupsOfWithStep = F3(
	function (size, step, xs) {
		var xs_ = A2($elm$core$List$drop, step, xs);
		var thisGroup = A2($elm$core$List$take, size, xs);
		var okayLength = _Utils_eq(
			size,
			$elm$core$List$length(thisGroup));
		var okayArgs = (size > 0) && (step > 0);
		return (okayArgs && okayLength) ? A2(
			$elm$core$List$cons,
			thisGroup,
			A3($elm_community$list_extra$List$Extra$groupsOfWithStep, size, step, xs_)) : _List_Nil;
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
var $author$project$WordOrPhrase$member = function (wopKey) {
	return $elm$core$Set$member(
		$author$project$WordOrPhrase$removeTashkyl(wopKey));
};
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$markPhrases = F2(
	function (allPhrasesWithLengths, list) {
		var wordsWithIndices = A2(
			$elm$core$List$filterMap,
			function (_v9) {
				var i = _v9.a;
				var wordOrNonWord = _v9.b;
				if (wordOrNonWord.$ === 'DisplayWord') {
					var word = wordOrNonWord.a;
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(word, i));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			},
			A2(
				$elm$core$List$indexedMap,
				F2(
					function (i, x) {
						return _Utils_Tuple2(i, x);
					}),
				list));
		var wordToPhraseWord = function (wdt) {
			if (wdt.$ === 'DisplayWord') {
				var word = wdt.a;
				return $author$project$WordDisplay$DisplayWordOfPhrase(word);
			} else {
				return wdt;
			}
		};
		var wordGroupsOfLength = function (n) {
			return A3($elm_community$list_extra$List$Extra$groupsOfWithStep, n, 1, wordsWithIndices);
		};
		var sortedLengths = $elm$core$List$sort(
			A2($elm$core$List$map, $elm$core$Tuple$second, allPhrasesWithLengths));
		var phraseSet = $elm$core$Set$fromList(
			A2($elm$core$List$map, $elm$core$Tuple$first, allPhrasesWithLengths));
		var matchingPhrases = A2(
			$elm$core$List$sortBy,
			function (_v6) {
				var _v7 = _v6.b;
				var startI = _v7.a;
				return startI;
			},
			A2(
				$elm$core$List$concatMap,
				function (phraseSize) {
					return A2(
						$elm$core$List$filterMap,
						function (possiblePhraseMatch) {
							var phrase = A2(
								$elm$core$String$join,
								' ',
								A2($elm$core$List$map, $elm$core$Tuple$first, possiblePhraseMatch));
							return A2($author$project$WordOrPhrase$member, phrase, phraseSet) ? $elm$core$Maybe$Just(
								_Utils_Tuple2(
									possiblePhraseMatch,
									function (l) {
										return _Utils_Tuple2(
											A2(
												$elm$core$Maybe$withDefault,
												0,
												$elm$core$List$head(l)),
											A2(
												$elm$core$Maybe$withDefault,
												0,
												$elm_community$list_extra$List$Extra$last(l)));
									}(
										A2($elm$core$List$map, $elm$core$Tuple$second, possiblePhraseMatch)))) : $elm$core$Maybe$Nothing;
						},
						wordGroupsOfLength(phraseSize));
				},
				sortedLengths));
		return function (_v5) {
			var x = _v5.a;
			return x;
		}(
			A3(
				$elm$core$List$foldr,
				F2(
					function (_v0, _v1) {
						var i = _v0.a;
						var wordOrNonWord = _v0.b;
						var ls = _v1.a;
						var mnextMatchingPhrase = _v1.b;
						var restPhrases = _v1.c;
						if (mnextMatchingPhrase.$ === 'Just') {
							var _v3 = mnextMatchingPhrase.a;
							var _v4 = _v3.b;
							var startI = _v4.a;
							var endI = _v4.b;
							return ((_Utils_cmp(i, startI) > -1) && (_Utils_cmp(i, endI) < 0)) ? _Utils_Tuple3(
								A2(
									$elm$core$List$cons,
									wordToPhraseWord(wordOrNonWord),
									ls),
								mnextMatchingPhrase,
								restPhrases) : (_Utils_eq(i, endI) ? _Utils_Tuple3(
								A2(
									$elm$core$List$cons,
									wordToPhraseWord(wordOrNonWord),
									ls),
								$elm$core$List$head(restPhrases),
								A2(
									$elm$core$Maybe$withDefault,
									_List_Nil,
									$elm$core$List$tail(restPhrases))) : _Utils_Tuple3(
								A2($elm$core$List$cons, wordOrNonWord, ls),
								mnextMatchingPhrase,
								restPhrases));
						} else {
							return _Utils_Tuple3(
								A2($elm$core$List$cons, wordOrNonWord, ls),
								$elm$core$Maybe$Nothing,
								_List_Nil);
						}
					}),
				_Utils_Tuple3(
					_List_Nil,
					$elm$core$List$head(matchingPhrases),
					A2(
						$elm$core$Maybe$withDefault,
						_List_Nil,
						$elm$core$List$tail(matchingPhrases))),
				A2(
					$elm$core$List$indexedMap,
					F2(
						function (i, x) {
							return _Utils_Tuple2(i, x);
						}),
					list)));
	});
var $elm$html$Html$Events$onMouseDown = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mousedown',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onMouseLeave = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseleave',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onMouseOver = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseover',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$Events$onMouseUp = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseup',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$Main$displayWords = F3(
	function (model, lessonText, lessonTitle) {
		var displayWord = F4(
			function (word, li, wi, partOfPhrase) {
				return A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('word', true),
									_Utils_Tuple2(
									'selected',
									A2($author$project$WordOrPhrase$tashkylEquivalent, model.selectedWop, word)),
									_Utils_Tuple2(
									'in-dict',
									$elm_community$maybe_extra$Maybe$Extra$isJust(
										A2($author$project$WordOrPhrase$get, word, model.wops))),
									_Utils_Tuple2('part-of-phrase', partOfPhrase)
								])),
							$elm$html$Html$Attributes$class(
							$elm$core$String$toLower(
								$author$project$WordOrPhrase$displayFamiliarityLevel(
									A2($author$project$WordOrPhrase$getFamiliarityLevel, word, model.wops)))),
							$elm$html$Html$Events$onClick(
							A2($author$project$WordOrPhrase$tashkylEquivalent, model.selectedWop, word) ? $author$project$Main$DeselectWOP : $author$project$Main$GetCurrentTimeAndThen(
								$author$project$Main$SelectWOP(
									{lessonId: lessonTitle, wopKey: word}))),
							$elm$html$Html$Events$onMouseDown(
							A3($author$project$Main$MouseDownOnWord, li, wi, word)),
							$elm$html$Html$Events$onMouseUp(
							A3($author$project$Main$OpenPhraseCreationUI, li, wi, word)),
							$elm$html$Html$Events$onMouseOver(
							$author$project$Main$WordHoverStart(word)),
							$elm$html$Html$Events$onMouseLeave($author$project$Main$WordHoverLeave)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(word)
						]));
			});
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('displayed-words')
				]),
			A2(
				$elm$core$List$indexedMap,
				F2(
					function (li, line) {
						return A2(
							$elm$html$Html$p,
							_List_Nil,
							A2(
								$elm$core$List$indexedMap,
								F2(
									function (wi, chunk) {
										switch (chunk.$) {
											case 'DisplayWord':
												var word = chunk.a;
												return A4(displayWord, word, li, wi, false);
											case 'DisplayNonWord':
												var nonWord = chunk.a;
												return A2(
													$elm$html$Html$span,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$class('non-word')
														]),
													_List_fromArray(
														[
															$elm$html$Html$text(nonWord)
														]));
											case 'DisplayWordOfPhrase':
												var word = chunk.a;
												return A4(displayWord, word, li, wi, true);
											default:
												return A2($elm$html$Html$div, _List_Nil, _List_Nil);
										}
									}),
								A2(
									$author$project$Main$markPhrases,
									$author$project$WordOrPhrase$allPhrases(model.wops),
									$author$project$WordDisplay$markWordCharsFromNonWordChars(line))));
					}),
				$author$project$Main$splitIntoCleanLines(lessonText)));
	});
var $author$project$Main$FinishFlashcardSession = {$: 'FinishFlashcardSession'};
var $author$project$Main$FlipLessonFlashcard = {$: 'FlipLessonFlashcard'};
var $author$project$Main$NextLessonFlashcard = F3(
	function (a, b, c) {
		return {$: 'NextLessonFlashcard', a: a, b: b, c: c};
	});
var $author$project$Main$familiarityLevelSelectorView = function (wop) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('familiarity-level-selector')
			]),
		A2(
			$elm$core$List$map,
			function (n) {
				return A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'selected',
									_Utils_eq(wop.familiarityLevel, n))
								])),
							$elm$html$Html$Attributes$class(
							$elm$core$String$toLower(
								$author$project$WordOrPhrase$displayFamiliarityLevel(n))),
							$elm$html$Html$Events$onClick(
							A2(
								$author$project$Main$SetSelectedWOPFamiliarityLevel,
								$author$project$WordOrPhrase$key(wop),
								n)),
							$elm$html$Html$Attributes$title(
							$author$project$WordOrPhrase$displayFamiliarityLevel(n))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(n))
						]));
			},
			_List_fromArray(
				[1, 2, 3, 4])));
};
var $author$project$Flashcard$getMostRecentHistory = function (card) {
	return $elm$core$List$head(card.history);
};
var $author$project$Flashcard$getTimeOfLastReview = function (cards) {
	return A2(
		$elm$core$Maybe$withDefault,
		0,
		A2(
			$elm$core$Maybe$map,
			function ($) {
				return $.timestamp;
			},
			A2(
				$elm$core$Maybe$andThen,
				$author$project$Flashcard$getMostRecentHistory,
				$elm_community$list_extra$List$Extra$last(cards))));
};
var $author$project$Flashcard$sToMs = function (seconds) {
	return 1000 * seconds;
};
var $author$project$Flashcard$getNextCard = function (cards) {
	var lastReview = A2(
		$elm$core$Debug$log,
		'last review',
		$author$project$Flashcard$getTimeOfLastReview(cards));
	var jumpAheadCandidate = A2(
		$elm_community$list_extra$List$Extra$find,
		function (card) {
			var _v1 = $author$project$Flashcard$getMostRecentHistory(card);
			if (_v1.$ === 'Just') {
				var history = _v1.a;
				var _v2 = A2($elm$core$Debug$log, 'history', history);
				return (!history.remembered) && (_Utils_cmp(
					lastReview - history.timestamp,
					$author$project$Flashcard$sToMs(60)) > 0);
			} else {
				return false;
			}
		},
		cards);
	if (jumpAheadCandidate.$ === 'Just') {
		var c = jumpAheadCandidate.a;
		return $elm$core$Maybe$Just(c);
	} else {
		return $elm$core$List$head(cards);
	}
};
var $author$project$Main$lessonPrepFlashcardsView = F2(
	function (model, flashcards) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('flashcard-view')
				]),
			_List_fromArray(
				[
					function () {
					var _v0 = $author$project$Flashcard$getNextCard(flashcards);
					if (_v0.$ === 'Just') {
						var flashcard = _v0.a;
						var _v1 = flashcard.data;
						var wop = _v1.a;
						var sentence = _v1.b;
						var sentenceView = A2(
							$elm$core$List$map,
							function (wdt) {
								switch (wdt.$) {
									case 'DisplayWord':
										var w = wdt.a;
										return _Utils_eq(
											w,
											$author$project$WordOrPhrase$key(wop)) ? A2(
											$elm$html$Html$span,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('target-wop')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(w)
												])) : A2(
											$elm$html$Html$span,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(w)
												]));
									case 'DisplayNonWord':
										var w = wdt.a;
										return A2(
											$elm$html$Html$span,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text(w)
												]));
									default:
										return A2($elm$html$Html$span, _List_Nil, _List_Nil);
								}
							},
							$author$project$WordDisplay$markWordCharsFromNonWordChars(sentence));
						return model.flashcardFlipped ? A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('flashcard-back')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('definitions')
										]),
									A2(
										$elm$core$List$map,
										function (def) {
											return $elm$html$Html$text(def);
										},
										wop.definitions)),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('sentence')
										]),
									sentenceView),
									$author$project$Main$familiarityLevelSelectorView(wop),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('response-buttons')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$button,
											_List_fromArray(
												[
													$elm$html$Html$Events$onClick(
													$author$project$Main$GetCurrentTimeAndThen(
														A2($author$project$Main$NextLessonFlashcard, flashcard, true)))
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Remembered')
												])),
											A2(
											$elm$html$Html$button,
											_List_fromArray(
												[
													$elm$html$Html$Events$onClick(
													$author$project$Main$GetCurrentTimeAndThen(
														A2($author$project$Main$NextLessonFlashcard, flashcard, false)))
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Forgot')
												]))
										]))
								])) : A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('flashcard-front')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('sentence')
										]),
									sentenceView),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('romanization')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(wop.romanization)
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Events$onClick($author$project$Main$FlipLessonFlashcard)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Flip Card')
										]))
								]));
					} else {
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Events$onClick($author$project$Main$FinishFlashcardSession)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Continue to Lesson')
								]));
					}
				}()
				]));
	});
var $author$project$Main$AddTranslationToSelectedLesson = {$: 'AddTranslationToSelectedLesson'};
var $author$project$Main$EditTranslationOfSelectedLesson = function (a) {
	return {$: 'EditTranslationOfSelectedLesson', a: a};
};
var $author$project$Main$lessonTranslationBox = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('lesson-translation-box')
			]),
		_List_fromArray(
			[
				function () {
				var _v0 = A2($elm$core$Dict$get, model.selectedLesson, model.lessonTranslations);
				if (_v0.$ === 'Nothing') {
					return A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick($author$project$Main$AddTranslationToSelectedLesson)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Add Translation')
							]));
				} else {
					var translation = _v0.a;
					return A2(
						$elm$html$Html$textarea,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$rows(1),
								$elm$html$Html$Attributes$cols(1),
								$elm$html$Html$Attributes$value(translation),
								$elm$html$Html$Events$onInput($author$project$Main$EditTranslationOfSelectedLesson)
							]),
						_List_Nil);
				}
			}()
			]));
};
var $author$project$Main$EditSelectedNewWOPDefinition = function (a) {
	return {$: 'EditSelectedNewWOPDefinition', a: a};
};
var $author$project$Main$EditSelectedWOPDefinition = F2(
	function (a, b) {
		return {$: 'EditSelectedWOPDefinition', a: a, b: b};
	});
var $author$project$Main$EditSelectedWOPNotes = function (a) {
	return {$: 'EditSelectedWOPNotes', a: a};
};
var $author$project$Main$EditSelectedWOPRomanization = function (a) {
	return {$: 'EditSelectedWOPRomanization', a: a};
};
var $author$project$Main$SaveSelectedNewWOP = {$: 'SaveSelectedNewWOP'};
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $author$project$Main$selectedWordEdit = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('selected-word-edit')
			]),
		function () {
			if ($elm$core$String$isEmpty(model.selectedWop)) {
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$p,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Select a word to view options')
							]))
					]);
			} else {
				var _v0 = A2($author$project$WordOrPhrase$get, model.selectedWop, model.wops);
				if (_v0.$ === 'Just') {
					var wop = _v0.a;
					return A2(
						$elm$core$List$cons,
						A2(
							$elm$html$Html$p,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('primary-definition')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('egyptian')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(model.selectedWop)
										])),
									$elm$html$Html$text(
									' = ' + A2($elm$core$String$join, ' || ', wop.definitions))
								])),
						_Utils_ap(
							A2(
								$elm$core$List$indexedMap,
								F2(
									function (i, def) {
										return A2(
											$elm$html$Html$input,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$placeholder(
													'Definition #' + $elm$core$String$fromInt(i + 1)),
													$elm$html$Html$Attributes$value(def),
													$elm$html$Html$Events$onInput(
													$author$project$Main$EditSelectedWOPDefinition(i))
												]),
											_List_Nil);
									}),
								_Utils_ap(
									wop.definitions,
									_List_fromArray(
										['']))),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$input,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$placeholder('romanization'),
											$elm$html$Html$Attributes$value(wop.romanization),
											$elm$html$Html$Events$onInput($author$project$Main$EditSelectedWOPRomanization),
											$elm$html$Html$Attributes$class('romanization')
										]),
									_List_Nil),
									A2(
									$elm$html$Html$textarea,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$cols(20),
											$elm$html$Html$Attributes$rows(10),
											$elm$html$Html$Events$onInput($author$project$Main$EditSelectedWOPNotes),
											$elm$html$Html$Attributes$value(wop.notes)
										]),
									_List_Nil),
									$author$project$Main$familiarityLevelSelectorView(wop)
								])));
				} else {
					return _List_fromArray(
						[
							A2(
							$elm$html$Html$p,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('primary-definition')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('egyptian')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(model.selectedWop)
										]))
								])),
							A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$id('newWopDefinition'),
									$elm$html$Html$Attributes$placeholder('add a definition'),
									$elm$html$Html$Events$onInput($author$project$Main$EditSelectedNewWOPDefinition),
									$elm$html$Html$Attributes$value(model.newWopDefinition)
								]),
							_List_Nil),
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Events$onClick($author$project$Main$SaveSelectedNewWOP),
									$elm$html$Html$Attributes$disabled(
									$elm$core$String$isEmpty(model.newWopDefinition))
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Save New Word')
								]))
						]);
				}
			}
		}());
};
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $author$project$Main$selectedLessonView = F2(
	function (model, title) {
		var mlesson = A2($elm$core$Dict$get, title, model.lessons);
		var lessonText = A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.text;
				},
				mlesson));
		var lessonAudioType = A2(
			$elm$core$Maybe$withDefault,
			'wav',
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.audioFileType;
				},
				mlesson));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('selected-lesson-view')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$h2,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Title: ' + title)
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('audio-container')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$audio,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$controls(true),
									$elm$html$Html$Attributes$src('http://localhost:3000/audio/' + (title + ('.' + lessonAudioType)))
								]),
							_List_Nil)
						])),
					$elm_community$maybe_extra$Maybe$Extra$isNothing(model.lessonFlashcards) ? A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(
							$author$project$Main$PrepareForLessonWithFlashcards(lessonText))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Prepare for Lesson with Flashcards')
						])) : A2($elm$html$Html$span, _List_Nil, _List_Nil),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(
							$author$project$Main$GetCurrentTimeAndThen($author$project$Main$MarkLessonAsReviewed))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Mark Lesson as Reviewed')
						])),
					function () {
					var _v0 = model.lessonFlashcards;
					if (_v0.$ === 'Nothing') {
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('lesson-words-and-lookup')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('selected-word-edit-and-lesson-translation')
										]),
									_List_fromArray(
										[
											$author$project$Main$selectedWordEdit(model),
											$author$project$Main$lessonTranslationBox(model)
										])),
									A3($author$project$Main$displayWords, model, lessonText, title)
								]));
					} else {
						var flashcards = _v0.a;
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('lesson-flashcard-view')
								]),
							_List_fromArray(
								[
									A2($author$project$Main$lessonPrepFlashcardsView, model, flashcards)
								]));
					}
				}()
				]));
	});
var $author$project$EnterNewWops$EditWOPDefinition = function (a) {
	return {$: 'EditWOPDefinition', a: a};
};
var $author$project$EnterNewWops$EditWOPKey = function (a) {
	return {$: 'EditWOPKey', a: a};
};
var $author$project$EnterNewWops$GoBack = {$: 'GoBack'};
var $author$project$EnterNewWops$SaveNewWOP = {$: 'SaveNewWOP'};
var $elm$html$Html$ol = _VirtualDom_node('ol');
var $elm_community$list_extra$List$Extra$isPrefixOf = F2(
	function (prefix, list) {
		isPrefixOf:
		while (true) {
			var _v0 = _Utils_Tuple2(prefix, list);
			if (!_v0.a.b) {
				return true;
			} else {
				if (!_v0.b.b) {
					var _v1 = _v0.a;
					return false;
				} else {
					var _v2 = _v0.a;
					var p = _v2.a;
					var ps = _v2.b;
					var _v3 = _v0.b;
					var x = _v3.a;
					var xs = _v3.b;
					if (_Utils_eq(p, x)) {
						var $temp$prefix = ps,
							$temp$list = xs;
						prefix = $temp$prefix;
						list = $temp$list;
						continue isPrefixOf;
					} else {
						return false;
					}
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$isInfixOfHelp = F3(
	function (infixHead, infixTail, list) {
		isInfixOfHelp:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (_Utils_eq(x, infixHead) && A2($elm_community$list_extra$List$Extra$isPrefixOf, infixTail, xs)) {
					return true;
				} else {
					var $temp$infixHead = infixHead,
						$temp$infixTail = infixTail,
						$temp$list = xs;
					infixHead = $temp$infixHead;
					infixTail = $temp$infixTail;
					list = $temp$list;
					continue isInfixOfHelp;
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$isInfixOf = F2(
	function (infixList, list) {
		if (!infixList.b) {
			return true;
		} else {
			var x = infixList.a;
			var xs = infixList.b;
			return A3($elm_community$list_extra$List$Extra$isInfixOfHelp, x, xs, list);
		}
	});
var $author$project$WordOrPhrase$searchWop = F2(
	function (wopKey, wops) {
		var charsWithoutTashkyl = function (str) {
			return $elm$core$String$toList(
				$author$project$WordOrPhrase$removeTashkyl(str));
		};
		return $elm$core$String$isEmpty(wopKey) ? _List_Nil : A2(
			$elm$core$List$sortBy,
			function (wop) {
				return A2(
					$elm_community$list_extra$List$Extra$isPrefixOf,
					charsWithoutTashkyl(wopKey),
					charsWithoutTashkyl(
						$author$project$WordOrPhrase$key(wop))) ? 0 : 1;
			},
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$second,
				A2(
					$elm$core$List$filter,
					function (_v0) {
						var k = _v0.a;
						return A2(
							$elm_community$list_extra$List$Extra$isInfixOf,
							charsWithoutTashkyl(wopKey),
							charsWithoutTashkyl(k));
					},
					$elm$core$Dict$toList(wops))));
	});
var $author$project$EnterNewWops$wopAlreadyExists = F2(
	function (_v0, parent) {
		var createdWops = _v0.createdWops;
		var newWop = _v0.newWop;
		return A2(
			$elm$core$List$any,
			$elm_community$maybe_extra$Maybe$Extra$isJust,
			_List_fromArray(
				[
					A2(
					$author$project$WordOrPhrase$get,
					$author$project$WordOrPhrase$key(newWop),
					$author$project$WordOrPhrase$intoDict(createdWops)),
					A2(
					$author$project$WordOrPhrase$get,
					$author$project$WordOrPhrase$key(newWop),
					parent.wops)
				]));
	});
var $author$project$EnterNewWops$view = F2(
	function (model, parent) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('enter-new-wops')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('input-container')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('egyptian'),
									$elm$html$Html$Attributes$placeholder('egyptian'),
									$elm$html$Html$Events$onInput($author$project$EnterNewWops$EditWOPKey),
									$elm$html$Html$Attributes$value(
									$author$project$WordOrPhrase$key(model.newWop))
								]),
							_List_Nil),
							A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('english'),
									$elm$html$Html$Attributes$placeholder('definition'),
									$elm$html$Html$Events$onInput($author$project$EnterNewWops$EditWOPDefinition),
									$elm$html$Html$Attributes$value(
									A2($elm$core$String$join, '', model.newWop.definitions))
								]),
							_List_Nil)
						])),
					A2($author$project$EnterNewWops$wopAlreadyExists, model, parent) ? A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('this wop already exists!')
						])) : A2(
					$elm$html$Html$p,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'visibility', 'hidden')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('nothing')
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('save-new-wop'),
							$elm$html$Html$Events$onClick($author$project$EnterNewWops$SaveNewWOP),
							$elm$html$Html$Attributes$disabled(
							$elm$core$String$isEmpty(
								$author$project$WordOrPhrase$key(model.newWop)) || (A2($author$project$EnterNewWops$wopAlreadyExists, model, parent) || $elm$core$String$isEmpty(
								A2($elm$core$String$join, '', model.newWop.definitions))))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Save New WOP')
						])),
					(!$elm$core$List$isEmpty(
					A2(
						$author$project$WordOrPhrase$searchWop,
						$author$project$WordOrPhrase$key(model.newWop),
						parent.wops))) ? A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('related wops: ')
						])) : A2($elm$html$Html$p, _List_Nil, _List_Nil),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('list')
						]),
					A2(
						$elm$core$List$map,
						function (wop) {
							return A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('egyptian')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												$author$project$WordOrPhrase$key(wop))
											])),
										$elm$html$Html$text(
										' : ' + A2($elm$core$String$join, ', ', wop.definitions))
									]));
						},
						A2(
							$author$project$WordOrPhrase$searchWop,
							$author$project$WordOrPhrase$key(model.newWop),
							parent.wops))),
					A2(
					$elm$html$Html$p,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('new wops')
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick($author$project$EnterNewWops$GoBack),
							$elm$html$Html$Attributes$disabled(
							$elm$core$List$isEmpty(model.createdWops))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('commit new wops and go back')
						])),
					A2(
					$elm$html$Html$ol,
					_List_Nil,
					A2(
						$elm$core$List$map,
						function (wop) {
							return A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$WordOrPhrase$key(wop)),
										$elm$html$Html$text(': '),
										$elm$html$Html$text(
										A2($elm$core$String$join, '', wop.definitions))
									]));
						},
						model.createdWops))
				]));
	});
var $author$project$Main$view = function (model) {
	return model.onFlashcardPage ? $author$project$Main$flashcardView(model) : (model.enterNewWops.viewingEnterNewWops ? A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h2,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Learn Egyptian')
					])),
				A2(
				$elm$html$Html$map,
				$author$project$Main$EnterNewWopsMsg,
				A2($author$project$EnterNewWops$view, model.enterNewWops, model))
			])) : A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h2,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Learn Egyptian')
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Main$SaveDataModelToClipboard)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Save Data Model to Clipboard (4MB limit)')
					])),
				$author$project$Main$newAndEditLessonView(model),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Main$NavigateToFlashcardPage)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Go to Flashcards Page')
					])),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Main$NavigateToEnterNewWops)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Go to Enter New Wops Page')
					])),
				$author$project$Main$lessonsView(model),
				$elm$core$String$isEmpty(model.selectedLesson) ? A2($elm$html$Html$span, _List_Nil, _List_Nil) : A2($author$project$Main$selectedLessonView, model, model.selectedLesson)
			])));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	A2(
		$elm$json$Json$Decode$andThen,
		function (wops) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (tick) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (newWopFlashcards) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (lessons) {
									return A2(
										$elm$json$Json$Decode$andThen,
										function (lessonTranslations) {
											return $elm$json$Json$Decode$succeed(
												{lessonTranslations: lessonTranslations, lessons: lessons, newWopFlashcards: newWopFlashcards, tick: tick, wops: wops});
										},
										A2(
											$elm$json$Json$Decode$field,
											'lessonTranslations',
											$elm$json$Json$Decode$list(
												A2(
													$elm$json$Json$Decode$andThen,
													function (_v0) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (_v1) {
																return $elm$json$Json$Decode$succeed(
																	_Utils_Tuple2(_v0, _v1));
															},
															A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$string));
													},
													A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string)))));
								},
								A2(
									$elm$json$Json$Decode$field,
									'lessons',
									$elm$json$Json$Decode$list(
										A2(
											$elm$json$Json$Decode$andThen,
											function (_v0) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (_v1) {
														return $elm$json$Json$Decode$succeed(
															_Utils_Tuple2(_v0, _v1));
													},
													A2(
														$elm$json$Json$Decode$index,
														1,
														A2(
															$elm$json$Json$Decode$andThen,
															function (text) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (audioFileType) {
																		return $elm$json$Json$Decode$succeed(
																			{audioFileType: audioFileType, text: text});
																	},
																	A2($elm$json$Json$Decode$field, 'audioFileType', $elm$json$Json$Decode$string));
															},
															A2($elm$json$Json$Decode$field, 'text', $elm$json$Json$Decode$string))));
											},
											A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string)))));
						},
						A2(
							$elm$json$Json$Decode$field,
							'newWopFlashcards',
							$elm$json$Json$Decode$oneOf(
								_List_fromArray(
									[
										$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
										A2(
										$elm$json$Json$Decode$map,
										$elm$core$Maybe$Just,
										A2(
											$elm$json$Json$Decode$andThen,
											function (current) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (before) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (after) {
																return $elm$json$Json$Decode$succeed(
																	{after: after, before: before, current: current});
															},
															A2(
																$elm$json$Json$Decode$field,
																'after',
																$elm$json$Json$Decode$list(
																	A2(
																		$elm$json$Json$Decode$andThen,
																		function (wordOrPhrase) {
																			return A2(
																				$elm$json$Json$Decode$andThen,
																				function (tags) {
																					return A2(
																						$elm$json$Json$Decode$andThen,
																						function (romanization) {
																							return A2(
																								$elm$json$Json$Decode$andThen,
																								function (reviewHistory) {
																									return A2(
																										$elm$json$Json$Decode$andThen,
																										function (notes) {
																											return A2(
																												$elm$json$Json$Decode$andThen,
																												function (familiarityLevel) {
																													return A2(
																														$elm$json$Json$Decode$andThen,
																														function (definitions) {
																															return $elm$json$Json$Decode$succeed(
																																{definitions: definitions, familiarityLevel: familiarityLevel, notes: notes, reviewHistory: reviewHistory, romanization: romanization, tags: tags, wordOrPhrase: wordOrPhrase});
																														},
																														A2(
																															$elm$json$Json$Decode$field,
																															'definitions',
																															$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
																												},
																												A2($elm$json$Json$Decode$field, 'familiarityLevel', $elm$json$Json$Decode$int));
																										},
																										A2($elm$json$Json$Decode$field, 'notes', $elm$json$Json$Decode$string));
																								},
																								A2(
																									$elm$json$Json$Decode$field,
																									'reviewHistory',
																									$elm$json$Json$Decode$list(
																										A2(
																											$elm$json$Json$Decode$andThen,
																											function (timestamp) {
																												return A2(
																													$elm$json$Json$Decode$andThen,
																													function (lessonId) {
																														return $elm$json$Json$Decode$succeed(
																															{lessonId: lessonId, timestamp: timestamp});
																													},
																													A2(
																														$elm$json$Json$Decode$field,
																														'lessonId',
																														$elm$json$Json$Decode$oneOf(
																															_List_fromArray(
																																[
																																	$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
																																	A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, $elm$json$Json$Decode$string)
																																]))));
																											},
																											A2($elm$json$Json$Decode$field, 'timestamp', $elm$json$Json$Decode$int)))));
																						},
																						A2($elm$json$Json$Decode$field, 'romanization', $elm$json$Json$Decode$string));
																				},
																				A2(
																					$elm$json$Json$Decode$field,
																					'tags',
																					$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
																		},
																		A2(
																			$elm$json$Json$Decode$field,
																			'wordOrPhrase',
																			$elm$json$Json$Decode$list($elm$json$Json$Decode$string))))));
													},
													A2(
														$elm$json$Json$Decode$field,
														'before',
														$elm$json$Json$Decode$list(
															A2(
																$elm$json$Json$Decode$andThen,
																function (wordOrPhrase) {
																	return A2(
																		$elm$json$Json$Decode$andThen,
																		function (tags) {
																			return A2(
																				$elm$json$Json$Decode$andThen,
																				function (romanization) {
																					return A2(
																						$elm$json$Json$Decode$andThen,
																						function (reviewHistory) {
																							return A2(
																								$elm$json$Json$Decode$andThen,
																								function (notes) {
																									return A2(
																										$elm$json$Json$Decode$andThen,
																										function (familiarityLevel) {
																											return A2(
																												$elm$json$Json$Decode$andThen,
																												function (definitions) {
																													return $elm$json$Json$Decode$succeed(
																														{definitions: definitions, familiarityLevel: familiarityLevel, notes: notes, reviewHistory: reviewHistory, romanization: romanization, tags: tags, wordOrPhrase: wordOrPhrase});
																												},
																												A2(
																													$elm$json$Json$Decode$field,
																													'definitions',
																													$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
																										},
																										A2($elm$json$Json$Decode$field, 'familiarityLevel', $elm$json$Json$Decode$int));
																								},
																								A2($elm$json$Json$Decode$field, 'notes', $elm$json$Json$Decode$string));
																						},
																						A2(
																							$elm$json$Json$Decode$field,
																							'reviewHistory',
																							$elm$json$Json$Decode$list(
																								A2(
																									$elm$json$Json$Decode$andThen,
																									function (timestamp) {
																										return A2(
																											$elm$json$Json$Decode$andThen,
																											function (lessonId) {
																												return $elm$json$Json$Decode$succeed(
																													{lessonId: lessonId, timestamp: timestamp});
																											},
																											A2(
																												$elm$json$Json$Decode$field,
																												'lessonId',
																												$elm$json$Json$Decode$oneOf(
																													_List_fromArray(
																														[
																															$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
																															A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, $elm$json$Json$Decode$string)
																														]))));
																									},
																									A2($elm$json$Json$Decode$field, 'timestamp', $elm$json$Json$Decode$int)))));
																				},
																				A2($elm$json$Json$Decode$field, 'romanization', $elm$json$Json$Decode$string));
																		},
																		A2(
																			$elm$json$Json$Decode$field,
																			'tags',
																			$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
																},
																A2(
																	$elm$json$Json$Decode$field,
																	'wordOrPhrase',
																	$elm$json$Json$Decode$list($elm$json$Json$Decode$string))))));
											},
											A2(
												$elm$json$Json$Decode$field,
												'current',
												A2(
													$elm$json$Json$Decode$andThen,
													function (wordOrPhrase) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (tags) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (romanization) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (reviewHistory) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (notes) {
																						return A2(
																							$elm$json$Json$Decode$andThen,
																							function (familiarityLevel) {
																								return A2(
																									$elm$json$Json$Decode$andThen,
																									function (definitions) {
																										return $elm$json$Json$Decode$succeed(
																											{definitions: definitions, familiarityLevel: familiarityLevel, notes: notes, reviewHistory: reviewHistory, romanization: romanization, tags: tags, wordOrPhrase: wordOrPhrase});
																									},
																									A2(
																										$elm$json$Json$Decode$field,
																										'definitions',
																										$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
																							},
																							A2($elm$json$Json$Decode$field, 'familiarityLevel', $elm$json$Json$Decode$int));
																					},
																					A2($elm$json$Json$Decode$field, 'notes', $elm$json$Json$Decode$string));
																			},
																			A2(
																				$elm$json$Json$Decode$field,
																				'reviewHistory',
																				$elm$json$Json$Decode$list(
																					A2(
																						$elm$json$Json$Decode$andThen,
																						function (timestamp) {
																							return A2(
																								$elm$json$Json$Decode$andThen,
																								function (lessonId) {
																									return $elm$json$Json$Decode$succeed(
																										{lessonId: lessonId, timestamp: timestamp});
																								},
																								A2(
																									$elm$json$Json$Decode$field,
																									'lessonId',
																									$elm$json$Json$Decode$oneOf(
																										_List_fromArray(
																											[
																												$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
																												A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, $elm$json$Json$Decode$string)
																											]))));
																						},
																						A2($elm$json$Json$Decode$field, 'timestamp', $elm$json$Json$Decode$int)))));
																	},
																	A2($elm$json$Json$Decode$field, 'romanization', $elm$json$Json$Decode$string));
															},
															A2(
																$elm$json$Json$Decode$field,
																'tags',
																$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
													},
													A2(
														$elm$json$Json$Decode$field,
														'wordOrPhrase',
														$elm$json$Json$Decode$list($elm$json$Json$Decode$string))))))
									]))));
				},
				A2($elm$json$Json$Decode$field, 'tick', $elm$json$Json$Decode$int));
		},
		A2(
			$elm$json$Json$Decode$field,
			'wops',
			$elm$json$Json$Decode$list(
				A2(
					$elm$json$Json$Decode$andThen,
					function (_v0) {
						return A2(
							$elm$json$Json$Decode$andThen,
							function (_v1) {
								return $elm$json$Json$Decode$succeed(
									_Utils_Tuple2(_v0, _v1));
							},
							A2(
								$elm$json$Json$Decode$index,
								1,
								A2(
									$elm$json$Json$Decode$andThen,
									function (wordOrPhrase) {
										return A2(
											$elm$json$Json$Decode$andThen,
											function (tags) {
												return A2(
													$elm$json$Json$Decode$andThen,
													function (romanization) {
														return A2(
															$elm$json$Json$Decode$andThen,
															function (reviewHistory) {
																return A2(
																	$elm$json$Json$Decode$andThen,
																	function (notes) {
																		return A2(
																			$elm$json$Json$Decode$andThen,
																			function (familiarityLevel) {
																				return A2(
																					$elm$json$Json$Decode$andThen,
																					function (definitions) {
																						return $elm$json$Json$Decode$succeed(
																							{definitions: definitions, familiarityLevel: familiarityLevel, notes: notes, reviewHistory: reviewHistory, romanization: romanization, tags: tags, wordOrPhrase: wordOrPhrase});
																					},
																					A2(
																						$elm$json$Json$Decode$field,
																						'definitions',
																						$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
																			},
																			A2($elm$json$Json$Decode$field, 'familiarityLevel', $elm$json$Json$Decode$int));
																	},
																	A2($elm$json$Json$Decode$field, 'notes', $elm$json$Json$Decode$string));
															},
															A2(
																$elm$json$Json$Decode$field,
																'reviewHistory',
																$elm$json$Json$Decode$list(
																	A2(
																		$elm$json$Json$Decode$andThen,
																		function (timestamp) {
																			return A2(
																				$elm$json$Json$Decode$andThen,
																				function (lessonId) {
																					return $elm$json$Json$Decode$succeed(
																						{lessonId: lessonId, timestamp: timestamp});
																				},
																				A2(
																					$elm$json$Json$Decode$field,
																					'lessonId',
																					$elm$json$Json$Decode$oneOf(
																						_List_fromArray(
																							[
																								$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
																								A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, $elm$json$Json$Decode$string)
																							]))));
																		},
																		A2($elm$json$Json$Decode$field, 'timestamp', $elm$json$Json$Decode$int)))));
													},
													A2($elm$json$Json$Decode$field, 'romanization', $elm$json$Json$Decode$string));
											},
											A2(
												$elm$json$Json$Decode$field,
												'tags',
												$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
									},
									A2(
										$elm$json$Json$Decode$field,
										'wordOrPhrase',
										$elm$json$Json$Decode$list($elm$json$Json$Decode$string)))));
					},
					A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$string))))))(0)}});}(this));