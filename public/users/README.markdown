## User Pages 💻

This directory contains FSLC members' own personal pages.

Generate your own by `cd`'ing into the root app directory and running `./make_user_page.sh <your_username>`.

Style it and add to the content the way you like, then create a pull request. Once merged, it will be available to the `/users` route.

## Acceptance Criteria ✅
If you have good intentions, this should be simple.

* Any javascript needs to be quick and easy to understand. Ideally, keep it less than or equal to 40 lines for fast approval.
* Under NO circumstances can cookies be touched.
* The only allowed access to `localStorage` is `'theme'` as read-only (e.g. `localStorage.getItem('theme')`).
* Stick to resources within your own `users` directory - anything else that shows up in the network tab of inspection tools will *most likely* result in a closed PR.
  + Anchor tags that link to external resources like GitHub are okay as long as the `href` attribute is not messed with in any javascript.
* Please keep the total size of all files in your directory less than 2MB.