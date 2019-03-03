# -*- coding: utf-8 -*-
"""
Created on Sun Jul 22 02:41:46 2018

@author: Maheshwari
"""

import pandas as pd
import numpy as np
from bokeh.plotting import figure
from bokeh.models.tools import HoverTool
from bokeh.models import ColumnDataSource
from bokeh.layouts import gridplot
from bokeh.transform import factor_cmap
from bokeh.embed import components
from flask import render_template, Flask
from bokeh.layouts import column, row
from bokeh.resources import INLINE
from datetime import datetime
from dateutil.parser import parse
from bokeh.models import LinearColorMapper, ColorBar
from bokeh.palettes import Viridis256
from bokeh.palettes import Paired12
from bokeh.palettes import Category20
from bokeh.core.properties import value

app = Flask(__name__)
data = pd.read_csv('landslide.csv')

datedata = data.event_date
def is_date(string):
    if string.find('.') == -1:
        return True
    else:
        return False
monthdata = []
yeardata = []
for i in range(datedata.shape[0]):
    if is_date(datedata[i]):
        monthdata.append(datetime.strptime(datedata[i], '%d-%m-%y %H:%M').month)
        yeardata.append(datetime.strptime(datedata[i], '%d-%m-%y %H:%M').year)
    else:
        monthdata.append(0)
        yeardata.append(0)
        
monthyear = pd.DataFrame({'month': monthdata, 'year': yeardata})
bymonth = monthyear.pivot_table(index = 'month', columns = 'year', aggfunc = len)
bymonth = bymonth.drop([0], axis = 1)
bymonth = bymonth.drop([0])
month = bymonth.index.tolist()
year = bymonth.columns
count = bymonth.values.flatten()
xname = []
yname = []
alpha = []
for i in range(len(month)):
    for j in range(len(year)):
        xname.append(month[i])
        yname.append(year[j])
        alpha.append(min(bymonth.iloc[i,j]/np.nanmax(count), 0.9) + 0.1)

for i in range(len(alpha)): 
    if np.isnan(alpha[i]):
        alpha[i] = 0

source = ColumnDataSource({'month': xname, 'year': yname, 'count': count, 'alpha': alpha})

color_mapper = LinearColorMapper(palette=Viridis256, low = np.nanmin(count), high = np.nanmax(count))
color_bar = ColorBar(color_mapper = color_mapper, label_standoff = 12, title = 'weight', location = (0,0))

p = figure(x_axis_location="below", tools="hover,save", x_range = [str(month[i]) for i in range(len(month))],
           tooltips = [('names', '@month, @year'), ('count', '@count')])

p.rect('month', 'year', 0.9, 0.9, source=source,
       color = {'field': 'count', 'transform': color_mapper}, alpha= 'alpha', line_color=None,
       hover_line_color='black', hover_color={'field': 'count', 'transform': color_mapper})
color_bar = ColorBar(color_mapper = color_mapper, label_standoff = 12, title = 'count', location = (0,0))
p.add_layout(color_bar, 'right')
p.grid.grid_line_color = None
p.axis.major_label_standoff = 0
p.xaxis.axis_label = "Month"
p.yaxis.axis_label = "Year"
p.height = 400

cumcount = bymonth.sum(skipna=True, axis = 1)
sourcecum = ColumnDataSource({'month': cumcount.index, 'count': cumcount})
x = cumcount.index.tolist()
#cyl_cmap = factor_cmap('month', palette=Paired12, factors=['1','2','3','4','5','6','7','8','9','10','11','12'])
q = figure(plot_height=250, x_range = [str(x[i]) for i in range(len(x))], title = "Cumulative Landslide Count over Months")
q.vbar(x=cumcount.index, top=cumcount, width=1, line_color="white", 
       fill_color=Paired12)
q.line(x=cumcount.index, y=cumcount)
q.xgrid.grid_line_color = None
q.circle(x=cumcount.index, y=cumcount)
q.y_range.start = 0
q.yaxis.axis_label = "Cumulative Count"
q.xaxis.visible = False
q.xgrid.visible = False
#layout = gridplot([[q],[p]])

loc_trig = data[['country_name', 'landslide_trigger']]
loc_trig_table = loc_trig.pivot_table(index = 'country_name', columns = 'landslide_trigger', aggfunc=len)
loc_trig_table = loc_trig_table.sort_index()
sumloctrig = loc_trig_table.sum(axis = 1, skipna=True)
ind30 = sumloctrig.sort_values(ascending=False).iloc[:30].index
loc_trig_table = loc_trig_table[loc_trig_table.index.isin(ind30)]

triggers = loc_trig_table.columns.tolist()
loc_trig_table[loc_trig_table.isnull()] = 0
r = figure(y_range=loc_trig_table.index.tolist(), tools="hover, wheel_zoom, pan", 
           tooltips="$name: @$name", plot_width = 1000, plot_height = 650,
          title = 'Landslide distribution over top 30 countries w.r.t triggers', toolbar_location = 'right')
r.hbar_stack(triggers, y='country_name', color=Category20[18], 
             source=loc_trig_table, 
             legend=[value(x) for x in triggers], height = 0.5)
r.x_range.start = 0
r.y_range.range_padding = 0.1
r.xgrid.grid_line_color = None
r.axis.minor_tick_line_color = None
r.outline_line_color = None
r.legend.location = "bottom_right"
r.legend.orientation = "vertical"

layout = row(gridplot([[q],[p]]), r)

@app.route('/')
def homepage():
    #q = get_plot(latlon)
    script, div = components(layout)

    #Render the page
    return render_template('Graph.html', script=script, div=div)    

if __name__ == '__main__':
    app.run(port=33507)