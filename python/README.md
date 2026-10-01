# Python FastAPI server

This is a Python implementation of the `stacserver` HTTP API. It reads the
same PostgreSQL/PostGIS tables and JSONB records as the R package, so existing
catalogs can be served without conversion. It implements the landing page,
conformance, collections, collection items, item lookup, and GET/POST search
routes, including bbox, datetime, Query-extension property filters, pagination,
optional asset signing, and optional CORS.

The implementation is split by responsibility: `app.py` configures FastAPI
and declares routes, `database.py` handles PostgreSQL access and search SQL,
`helpers.py` contains request parsing and STAC link helpers, and `schema.sql`
defines the compatible database schema.

## Install and run

```sh
cd python
python -m venv .venv
. .venv/bin/activate
pip install -r requirements.txt
export DATABASE_URL='postgresql://user:password@localhost:5432/stac'
export STAC_BASE_URL='http://localhost:8000'
uvicorn 'stacserver.app:create_app' --factory --host 0.0.0.0 --port 8000
```

The database must have the schema created by the R package's `stac_db_setup()`.
An equivalent idempotent schema is supplied in `stacserver/schema.sql`; the
database role must have permission to create PostGIS, or PostGIS must already
be installed. The server intentionally does not provide authentication;
protect it with an authenticated reverse proxy or gateway.

For embedding, call `create_app(database_url=..., base_url=..., title=...,
description=..., sign_fn=..., cors_origins=...)`. A psycopg
`ConnectionPool` can also be passed with `pool=...`. `sign_fn` takes one href
and returns the signed href. `cors_origins` accepts a list of origins, a single
origin, or `"*"`; omit it to send no CORS headers.

Search accepts `bbox`, `datetime`, `collections`, `ids`, `limit`, and `offset`.
POST `/search` additionally accepts `query` (or the legacy `properties` alias)
using the STAC Query extension operators `eq`, `neq`, `lt`, `lte`, `gt`,
`gte`, `startsWith`, `endsWith`, `contains`, and `in`.
