## User Pages 💻

This directory contains FSLC members' own personal pages!

Generate your own by `cd`'ing into the root app directory and running `./make_user_page.sh <your_username>`.

Style it and add to the content the way you like, then create a pull request. Once merged, it will be available to the `/users` route! 🚀🚀🚀

## Acceptance Criteria ✅
Let's try to keep this secure 🔒 but fun!

* Any javascript should be quick and easy to understand. Ideally, keep it less than or equal to 40 lines for fast approval.
* Don't touch cookies 🍪! (Cookie monster likes his cookies untampered-with )
* The only allowed access to `localStorage` is `'theme'` as read-only (e.g. `localStorage.getItem('theme')`).
* Stick to serving resources within your own `users` directory - anything will *most likely* result in a closed PR. Anchor tags that link to external resources like GitHub are okay as long as the `href` attribute is not messed with in any javascript.
* Attempt to do everything in less than a megabyte plz.
