# flora-api

The API for the Flora Creative website.  Currently it's incredibly
under-featured, only accepts GET requests to / in order to retrieve all apps
metadata, or GET requests to /cactus (etc.) to retrieve a specific app metadata.

Note: `src/Network` and `src/Text` are taken from
https://github.com/jtdaugherty/HaskellNet at commit
af38595ec1bd804bdc365d103c9142f595e6295c and modified slightly. The original
license has been reproduced alongside the source.

`src/Network/HaskellNet/SSL.hs`, `src/Network/HaskellNet/SSL/Internal.hs` and
`src/Network/HaskellNet/SMTP/SSL.hs` are taken from
https://github.com/dpwright/HaskellNet-SSL at commit
6fe62d91af3913eff1180dabeb740d98c856c220 and not modified except to add their
license.
