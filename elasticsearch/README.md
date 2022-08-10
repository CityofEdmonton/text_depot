## SSL Certificates

If your ElasticSearch server is behind HTTPS and you require certificates, then add the `.crt` files in the `elasticsearch/certificates` directory, and they will be installed in the docker image.

## Data Format

Each data source should be stored in its own Elastic Search index. The index must be defined as the following (note that the `*_vector` fields are optional, and only used in the embedding search). Use the following mappings to [create](https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-create-index.html#mappings) your indexes:

```
{
    "properties": {
        "date": {
            "type": "date"
        },
        "id": {
            "type": "long"
        },
        "neighbourhoods": {
            "type": "nested",
            "properties": {
                "count": {
                    "type": "long"
                },
                "name": {
                    "type": "text",
                    "fields": {
                        "keyword": {
                            "type": "keyword",
                            "ignore_above": 256
                        }
                    }
                },
                "ratio": {
                    "type": "float"
                }
            }
        },
        "parent_source_title_vector": {
            "type": "dense_vector",
            "dims": 256
        },
        "source_title_vector": {
            "type": "dense_vector",
            "dims": 256
        },
        "text_vector": {
            "type": "dense_vector",
            "dims": 256
        },
        "num_chars": {
            "type": "long"
        },
        "num_sentences": {
            "type": "long"
        },
        "parent_source_title": {
            "type": "text",
            "fields": {
                "keyword": {
                    "type": "keyword",
                    "ignore_above": 256
                }
            }
        },
        "parent_source_url": {
            "type": "text",
            "fields": {
                "keyword": {
                    "type": "keyword",
                    "ignore_above": 256
                }
            }
        },
        "sentiment_polarity": {
            "type": "float"
        },
        "sentiment_subjectivity": {
            "type": "float"
        },
        "source_title": {
            "type": "text",
            "fields": {
                "keyword": {
                    "type": "keyword",
                    "ignore_above": 256
                }
            }
        },
        "source_url": {
            "type": "text",
            "fields": {
                "keyword": {
                    "type": "keyword",
                    "ignore_above": 256
                }
            }
        },
        "text": {
            "type": "text",
            "fields": {
                "keyword": {
                    "type": "keyword",
                    "ignore_above": 256
                }
            }
        }
    }
}
```

Then, add your indexes/aliases to the `default_index_aliases` parameter in `.configs`.
