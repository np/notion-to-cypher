# notion-to-memgraph

Transforme un JSON issu de `POST /v1/databases/{database_id}/query` (Notion) en requête Cypher compatible Neo4j/Memgraph.

## Build

```bash
cabal update
cabal build
```

## Exécution

Entrée depuis un fichier ou stdin. Label par défaut: NotionPage.

# Fichier

`cabal run notion-to-memgraph -- --label NotionPage -i notion_query.json > graph.cypher`

# Stdin

`cat notion_query.json | cabal run notion-to-memgraph -- --label MyPage > graph.cypher`

Ajoutez `--json` pour obtenir un objet JSON de la forme `{"statements": [{"statement": "MATCH (n) RETURN n;"}]}` :

```
cat notion_query.json | cabal run notion-to-memgraph -- --json > queries.json
```

## Serveur Web

Un serveur HTTP minimal expose la transformation en webhook.

```
cabal run notion-to-memgraph-web
```

Envoyez une requête `POST` avec le JSON Notion sur `http://localhost:8080/?label=MonLabel`
pour recevoir la requête Cypher en réponse.

Ajoutez `json=1` à l'URL pour une réponse `application/json` de la forme `{"statements": [{"statement": "..."}]}` :

```
curl -XPOST 'http://localhost:8080/?json=1' --data @notion_query.json
```

## Docker

Un `Dockerfile` minimal construit l'exécutable web :

```
docker build -t notion-to-memgraph .
docker run -p 8080:8080 notion-to-memgraph
```

## Modèle de graphe

Un nœud page par élément `results[] : (:NotionPage {id, title, url, archived, created_time, last_edited_time, ...})`.

Projections scalaires pour rich_text, number, checkbox, url, email, phone_number, date (dépliée en _start/_end/_tz), formula (string/number/bool/date), unique_id (dépliée).

people → nœuds `(:NotionUser {id,name,user_type})` et arêtes `(page)-[:HAS_PERSON__{prop}]->(user)`.

select/multi_select/status → nœuds `(:NotionOption {id,name,color,property})` et arêtes `(page)-[:HAS_OPTION__{prop}]->(option)`.

relation → arêtes `(page)-[:REL__{prop}]->(page) (MERGE le nœud cible par id).`

Les labels, relations et noms de propriété sont entourés de backticks (les backticks internes sont doublés). Les chaînes sont échappées.
Limitations

*    Ne gère pas les rollups et quelques types exotiques non listés.

*    N’impose pas d’index/contraintes; à gérer dans votre base.
