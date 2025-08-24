# notion-to-memgraph

Transforme un JSON issu de `POST /v1/databases/{database_id}/query` (Notion) en requête Cypher compatible Neo4j/Memgraph.

## Build

```bash
cabal update
cabal build

Exécution

Entrée depuis un fichier ou stdin. Label par défaut: NotionPage.

# Fichier
cabal run notion-to-memgraph -- --label NotionPage -i notion_query.json > graph.cypher

# Stdin
cat notion_query.json | cabal run notion-to-memgraph -- --label MyPage > graph.cypher

Modèle de graphe

    Un nœud page par élément results[] : (:NotionPage {id, title, url, archived, created_time, last_edited_time, ...}).

    Projections scalaires pour rich_text, number, checkbox, url, email, phone_number, date (dépliée en _start/_end/_tz), formula (string/number/bool/date), unique_id (dépliée).

    people → nœuds (:NotionUser {id,name,user_type}) et arêtes (page)-[:HAS_PERSON__{prop}]->(user).

    select/multi_select/status → nœuds (:NotionOption {id,name,color,property}) et arêtes (page)-[:HAS_OPTION__{prop}]->(option).

    relation → arêtes (page)-[:REL__{prop}]->(page) (MERGE le nœud cible par id).

Toutes les clés/labels/relations sont assainis pour Cypher (sanitizeProp/Rel/Label). Les chaînes sont échappées.
Limitations

    Ne gère pas les rollups et quelques types exotiques non listés.

    N’impose pas d’index/contraintes; à gérer dans votre base.
    EOF

cat > LICENSE << 'EOF'
MIT License

Copyright (c) 2025

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
