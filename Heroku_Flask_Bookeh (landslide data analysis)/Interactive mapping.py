import pandas as pd
import numpy as np
from bokeh.io import output_notebook, show
from bokeh.plotting import figure
from bokeh.models.tools import HoverTool
from bokeh.models import ColumnDataSource
from bokeh.layouts import gridplot
from bokeh.layouts import widgetbox
from bokeh.models.widgets import Dropdown
from bokeh.models import CustomJS
from bokeh.models import WMTSTileSource
from bokeh.models.widgets import CheckboxButtonGroup
from bokeh.models import OpenURL, TapTool
from bokeh.transform import factor_cmap
from bokeh.embed import components
from flask import render_template, Flask
from bokeh.layouts import column
from bokeh.resources import INLINE 

app = Flask(__name__)
data = pd.read_csv('landslide.csv')
latlon = data[['country_name', 'latitude', 'longitude', 'landslide_size', 'source_link', 'location_description']]

def wgs84_to_web_mercator(df, lat="latitude", lon="longitude"):
        """Converts decimal longitude/latitude to Web Mercator format"""
        k = 6378137
        a1 = np.array(df[lon] * (k * np.pi/180.0))
        a2 = np.array(np.log(np.tan((90 + df[lat]) * np.pi/360.0)) * k)
        df.loc[:, "latitude"] = a1
        df.loc[:, "longitude"] = a2
        pd.options.mode.chained_assignment = None
        return df

latlon = wgs84_to_web_mercator(latlon)
max_lat = latlon['latitude'].max()
min_lat = latlon['latitude'].min()
max_lon = latlon['longitude'].max()
min_lon = latlon['longitude'].min()
x_range,y_range = ((min_lat, max_lat), (min_lon,max_lon))
latlon.loc[latlon.landslide_size.isnull(), 'landslide_size'] = 'unknown'

def get_plot(data):
    countries = data.country_name.unique().tolist()
    countries = [country for country in countries if str(country) != 'nan']
    countries.append(' All')
    countries.sort()
    
    p = figure(tools='pan, wheel_zoom, tap, reset', x_range=x_range, y_range=y_range, plot_width = 950,
               toolbar_location = "right", active_scroll = 'wheel_zoom' )
    p.axis.visible = False
    
    url = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{Z}/{Y}/{X}.jpg'
    attribution = "Tiles by Carto, under CC BY 3.0. Data by OSM, under ODbL"
    p.add_tile(WMTSTileSource(url=url, attribution=attribution))
    source = ColumnDataSource(dict(latitude = latlon.latitude, longitude = latlon.longitude, country_name = latlon.country_name,
                                   latitude_t = latlon.latitude, longitude_t = latlon.longitude, country_name_t = latlon.country_name,
                                   landslide_size = latlon.landslide_size, source_link = latlon.source_link,
                                   location_description = latlon.location_description))
    cmap = factor_cmap('landslide_size', palette=['#ffffff', '#f7a8e2', '#fcdc12', '#fa774c', '#FC3232', '#040100'], 
                                                  factors=latlon.landslide_size.unique().tolist())
    p.add_tools(HoverTool(tooltips='@location_description', mode = 'mouse'))
    url = "@source_link"
    taptool = p.select(type=TapTool)
    taptool.callback = OpenURL(url=url)
    p.circle(x='latitude', y='longitude', fill_color=cmap, size=8, source = source, 
             legend = 'landslide_size')
    
    return(p)



@app.route('/')
def homepage():
    q = get_plot(latlon)
    script, div = components(q)

    #Render the page
    return render_template('Graph.html', script=script, div=div)    

if __name__ == '__main__':
    app.run(port=33507) #Set to false when deploying