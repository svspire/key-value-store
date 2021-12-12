# key-value-store
Defines a generic API for Common Lisp key/value stores.

See included documentation file [key-value-store.html](http://htmlpreview.github.io/?https://github.com/svspire/key-value-store/blob/master/key-value-store.html).

The purpose of this library is to create a key/value store mechanism with consistent call and return semantics regardless of how the underlying store is implemented, which Common Lisp does not natively provide. By using the functions herein it's easy to change a key/value store from one storage mechanism to another with only one line of code. The built-in methods support alists and hashtables, but others can be added by adding specialized methods.

This allows one to override the default methods herein for common key/value stores (e.g. alists and hashtables) to also work on many other types of stores, and the calling semantics are consistent such that widespread changes are not needed in code that changes from one underlying storage mechanism to another.

Property lists are not supported herein, because it's not possible to distinguish a plist from an alist with method specialization, and I prefer alists. Any storage mechanism of type list is assumed to be an alist.

The other reason I wrote this is that setf semantics are often wrong for my purposes. In many cases, I don't care about the value I just set; I'd much rather have the store itself returned as the primary value because this makes composition easier. Therefore, the modifier functions herein (#'relate, #'relate-unique, #clear-store, #'remove-key, and #'tally) always return the store itself as their primary returned value. In the case of #'relate, a second boolean value is returned that is true if the value in the store was actually modified. In the case of #'tally--which is more special-purpose--the second value is the new tally amount.

The inquiry method #'lookup-key works much like #'gethash in common lisp; it returns the value matching a given key. We differ from #'gethash in that the first argument here is the store itself and the second is the key. Again, this makes composition easier and it also makes methods easier to specialize, since you're much more likely to want to specialize on the store and not the key, and it's usually more efficient and more aesthetic to have a defmethod's specialized argument come first. One additional difference in #'lookup-key is that it always accepts a default argument, like #'gethash but unlike #'assoc.

*Note 1:* It's always a good idea to call the modifier functions herein with a setf in front, e.g. (setf my-store (relate my-store key value)). If my-store is an alist, this is mandatory, because we don't guarantee that we destructively modify alists. We *can* but not always. If my-store is a hashtable, prepending setf is optional, because hashtables are always destructively modified. Allowing consistent use of prepended setf was part of what I meant above by making composition easier; without this semantics, changing a key/value store from an alist to a hashtable would have required code changes wherever the store was modified.

The macros relate!, relate-unique!, remove-key!, tally!, and clear-store! are the macro versions of their respective functions. Using them automatically prepends (setf store ...) in front of their respective functions. Naturally to use these macros the store argument must be something setf-able (i.e. not a literal alist, for example).

*Note 2:* For alist stores, we try not to cons up a new list in #'relate when we can avoid it. In other words, if the (key . value) pair in question is already present when #'relate is called, we don't cons up a new list. Likewise, if value is a member of the cdr of (key value1 value2 ...) in the alist, we don't cons. But in #'relate-unique, we don't check for an existing value, so we sometimes cons and we sometimes destructively modify the list.
