## Config

This directory contains the config constants that will be used in the application, similar to a `.env`, which is automatically read from the current environment variable "APP_ENV".conf.

## Strucutre

Each "section" is defined by a heading wrapped in brackets `[]`, and is parsed into the keyword symbol
`[section_name]` `=>` `:|section_name|`, and further, each property as defined within the section is parsed
`:property_name rest of the line is a string` `=>` `:|property_name|`, whose value in the section's hash-map
is `rest of the line is a string`.

If any value or section is missing from the configuration, which the application consumes, the hashmap
implementation guarantees it will be `nil`. E.g., removing the `:error-log` property in `[app-log]` will
treat the value for `:error-log` as `nil` (with the side effect that errors are not logged by lack).
This can also be replicated by setting the property to the value "nil" in the config.

"Formal" grammar:

```
CONFIG  => ε
        => SECTION\nCONFIG
SECTION => HEADER
        => HEADER\nPROP*
HEADER  => [NAME]
NAME    => \wNAME
        => -NAME
        => ε
PROP    => :NAME VALUE
VALUE   => [^#]* (anything besides # - anything after that is a comment)
```

Not sure how accurate this^ is as a grammar. Just look at the examples `:P`.
