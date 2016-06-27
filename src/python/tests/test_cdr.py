import nose.tools as nt

import pandas as pd
import cdr

def test_join_cdr_grid():

  df = [
      [1, 1383260400000, 0, 1, 1, 1, 1, 1],
      [1, 1383260400000, 39, 2, 2, 2, 2, 2],
      [1, 1383260400000, 33, 3, 3, 3, 3, 3]
  ]
  grid = {
      "crs": {
          "type": "name",
          "properties": {
              "name": "urn:ogc:def:crs:EPSG::4326"
          }
      },
      "type": "FeatureCollection",
      "features": [
          {
          "geometry": {
              "type": "Polygon",
              "coordinates": [
                [
                  [
                    9.0114910478323,
                    45.35880131440966
                  ],
                  [
                    9.014491488013135,
                    45.35880097314403
                  ],
                  [
                    9.0144909480813,
                    45.35668565341486
                  ],
                  [
                    9.011490619692509,
                    45.356685994655464
                  ],
                  [
                    9.0114910478323,
                    45.35880131440966
                  ]
                ]
              ]
            },
            "type": "Feature",
            "id": 0,
            "properties": {
              "cellId": 1
            }
          },
          {
            "geometry": {
              "type": "Polygon",
              "coordinates": [
                [
                  [
                    9.014491488013135,
                    45.35880097314403
                  ],
                  [
                    9.017491928134044,
                    45.358800553060284
                  ],
                  [
                    9.017491276410173,
                    45.35668523336193
                  ],
                  [
                    9.0144909480813,
                    45.35668565341486
                  ],
                  [
                    9.014491488013135,
                    45.35880097314403
                  ]
                ]
              ]
            },
            "type": "Feature",
            "id": 1,
            "properties": {
              "cellId": 2
            }
          }
      ]
  }

  df = pd.DataFrame(df)
  grid = pd.Series(grid)
  geojson = cdr.join_cdr_grid(df, grid)

  nt.assert_equal(geojson["features"][0]["properties"]["internet"], 5)
  nt.assert_equal(geojson["features"][0]["properties"]["smsIn"], 5)

