# stacserver: Serve SpatioTemporal Asset Catalogs over a STAC API

Serves STAC (SpatioTemporal Asset Catalog) metadata as a STAC API 1.0
compliant HTTP service. Catalog objects built with the 'stacbuildr'
package are ingested into a PostgreSQL/PostGIS database and exposed
through a 'plumber' router implementing the core, item search and
collections conformance classes. Optional helpers sign asset URLs for
private object storage on Azure Blob Storage, Google Cloud Storage and
Amazon S3.

## See also

Useful links:

- <https://github.com/stevenpawley/stacserver>

- <https://stevenpawley.github.io/stacserver/>

- Report bugs at <https://github.com/stevenpawley/stacserver/issues>

## Author

**Maintainer**: Steven Pawley <dr.stevenpawley@gmail.com>

Authors:

- Steven Pawley <dr.stevenpawley@gmail.com>
