## Haskell Implementation of the JSON-API specification

### Attribution

This codebase is a fork of the original [json-api](https://github.com/toddmohney/json-api) lib authored by [Todd Mohney](https://github.com/toddmohney).

#### The specification

Find the specification [here](http://jsonapi.org/)

#### Example usage

Let's start with a simple User resource record:

```Haskell
data UserResource = UserResource
  { resourceId        :: Text
  , emailAddress :: Text
  , firstName :: Text
  , middleName :: Maybe Text
  , lastName  :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''UserResource)

instance ResourcefulEntity UserResource where
  resourceIdentifier = resourceId
  resourceType _ = "users"
  resourceLinks = Just . JSONApi.showLink
  resourceMetaData _ = Nothing
  resourceRelationships _ = Nothing
```

From this, we can use the `json-api` package to produce a payload conformant
to the [JSON-API specification](http://jsonapi.org/) like so:

```Haskell
-- Import the JSON API
import qualified Network.JSONApi as JSONApi
-- Builds the Document which will be serialized as our
-- web server's response payload
let userResource = UserResource {
    resourceId = "8b384d842a8b33fdcaf6207ad45b62c9"
  , emailAddress = "john@doe.com"
  , firstName = "John"
  , middleName = Nothing
  , lastName = "Doe"
  }
JSONApi.mkDocument [userResource] Nothing Nothing
```

When delivered as a response from a web server, for example, we get a payload
that looks like this:

```JSON
{
  "data": {
      "attributes": {
          "resourceId": "8b384d842a8b33fdcaf6207ad45b62c9",
          "middleName": "Adrian",
          "lastName": "Doe",
          "emailAddress": "john@doe.com",
          "firstName": "John"
      },
      "relationships": null,
      "id": "8b384d842a8b33fdcaf6207ad45b62c9",
      "meta": null,
      "type": "users",
      "links": {
          "self": "/users/8b384d842a8b33fdcaf6207ad45b62c9"
      }
  },
  "meta": null,
  "included": [],
  "links": null
}
```

#### Example Project

There is an [example project](https://github.com/shirren/servant-store) illustrating how the library can be used in the context of a web server.

#### Hackage

Module documentation can be found on [Hackage](http://hackage.haskell.org/package/json-api-lib)
