import numpy as np
from sklearn.svm import SVC
import plotly.graph_objects as go
import dash
from dash import dcc, html
from dash.dependencies import Input, Output

def generate_checkerboard(n_samples, n_tiles, seed):
    np.random.seed(seed)
    X = np.random.uniform(0, 1, size=(n_samples, 2))
    y = ((np.floor(X[:, 0] * n_tiles) + np.floor(X[:, 1] * n_tiles)) % 2).astype(int)
    return X, y

# Inicializar o aplicativo Dash
app = dash.Dash(__name__)

# Layout do aplicativo
app.layout = html.Div([
    html.H1("Visualização SVM com Kernel RBF", style={'textAlign': 'center'}),
    
    html.Div([
        html.Div([
            html.Label("Parâmetro C:"),
            dcc.Dropdown(
                id='c-dropdown',
                options=[{'label': str(c), 'value': c} for c in [1, 10, 100]],
                value=10,
                clearable=False
            )
        ], style={'width': '30%', 'display': 'inline-block', 'padding': '10px'}),
        
        html.Div([
            html.Label("Parâmetro Gamma:"),
            dcc.Dropdown(
                id='gamma-dropdown',
                options=[{'label': str(gamma), 'value': gamma} for gamma in [0.1, 1, 10]],
                value=1,
                clearable=False
            )
        ], style={'width': '30%', 'display': 'inline-block', 'padding': '10px'}),
        
        html.Div([
            html.Label("Número de Tiles:"),
            dcc.Slider(
                id='n-tiles-slider',
                min=2,
                max=4,
                step=1,
                value=3,
                marks={2: '2', 3: '3', 4: '4'}
            )
        ], style={'width': '30%', 'display': 'inline-block', 'padding': '10px'})
    ]),
    
    dcc.Graph(id='svm-graph')
])

# Callback para atualizar o gráfico
@app.callback(
    Output('svm-graph', 'figure'),
    [Input('c-dropdown', 'value'),
     Input('gamma-dropdown', 'value'),
     Input('n-tiles-slider', 'value')]
)
def update_graph(C, gamma, n_tiles):
    # Gerar dados
    n_samples = 1000
    seed = 2
    X, y = generate_checkerboard(n_samples, n_tiles, seed)
    
    # Treinar modelo
    model = SVC(kernel='rbf', C=C, gamma=gamma)
    model.fit(X, y)
    
    # Criar figura
    fig = go.Figure()
    
    # Adicionar pontos de dados
    fig.add_trace(
        go.Scatter(
            x=X[:, 0],
            y=X[:, 1],
            mode='markers',
            marker=dict(
                color=y,
                colorscale='Bluered',
                size=11,
                line=dict(width=0.2, color='DarkSlateGrey')
            ),
            name='Dados',
            showlegend=False
        )
    )
    
    # Adicionar vetores de suporte
    fig.add_trace(
        go.Scatter(
            x=model.support_vectors_[:, 0],
            y=model.support_vectors_[:, 1],
            mode='markers',
            marker=dict(
                symbol='x',
                color='black',
                size=10,
                line=dict(width=0.5)
            ),
            name='Vetores de Suporte'
        )
    )
    
    # Atualizar layout
    fig.update_layout(
        plot_bgcolor='white', 
        paper_bgcolor='lightgray',
        title=f'SVM com C={C}, gamma={gamma}, n_tiles={n_tiles}',
        xaxis_title='Feature 1',
        yaxis_title='Feature 2',
        hovermode='closest',
        width=1200,
        height=800,
        margin=dict(l=50, r=50, b=50, t=50, pad=4),
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="right", x=1)
    )
    
    return fig

if __name__ == '__main__':
    app.run(debug=True)