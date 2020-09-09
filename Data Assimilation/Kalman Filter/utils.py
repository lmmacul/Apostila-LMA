import numpy as np
from collections import namedtuple
from matplotlib import pyplot as plt 
from filterpy.kalman import predict, update
from filterpy.stats import plot_covariance
from filterpy.stats import multivariate_multiply
from filterpy.stats import plot_covariance_ellipse

gaussian = namedtuple('Gaussian', ['mean', 'var'])
gaussian.__repr__ = lambda s: '𝒩(μ={:.3f}, 𝜎²={:.3f})'.format(s[0], s[1])

def gps_stationary_example(n_obs, location):
    location = location[:n_obs]
    
    mean = np.mean(location, axis = 0)

    plt.figure(figsize = (4,4), dpi = 100)
    plt.plot(location[:,0], location[:,1], 'xC3', markersize = 10, markeredgewidth = 3, label = "Medições")
    plt.plot(mean[0], mean[1], 'oC0', markersize = 10, alpha = 0.85, label = "Média")
    plt.xlim(-3, 3)
    plt.ylim(-3, 3)
    plt.legend()
    plt.show()
    
def pdf_gaussian(x_axis, gaussian):
    return np.exp(-(x_axis - gaussian.mean)**2 / (2 * gaussian.var)) / (np.sqrt(gaussian.var * 2 * np.pi))

def gaussian_multiply(x, z):
    mean = (x.var * z.mean + z.var * x.mean) / (x.var + z.var)
    var = x.var * z.var / (x.var + z.var)
    print("var_estimado = ",var)
    return gaussian(mean, var)

def gaussian_sum(x, z):
    mean = x.mean + z.mean
    var = x.var + z.var
    return gaussian(mean, var)

def plot_gaussian_multiplication(x_mean, x_var, z_mean, z_var):
    plt.figure(figsize = (9, 6))
    x_axis = np.linspace(-5, 5, 100)
    x = gaussian(x_mean, x_var)
    z = gaussian(z_mean, z_var)

    posterior = gaussian_multiply(x, z)
    plt.plot(x_axis, pdf_gaussian(x_axis, x), label = 'x')
    plt.plot(x_axis, pdf_gaussian(x_axis, z), label = 'z')
    plt.plot(x_axis, pdf_gaussian(x_axis, posterior), "-.")
    plt.legend(["x", "z", "Estimativa"])

def plot_gaussian_sum(x_mean, x_var, dx_mean, dx_var):
    plt.figure(figsize = (9, 6))
    x_axis = np.linspace(-5, 5, 100)
    x = gaussian(x_mean, x_var)
    dx = gaussian(dx_mean, dx_var)

    prior = gaussian_sum(x, dx)
    plt.plot(x_axis, pdf_gaussian(x_axis, x), label = 'x')
    plt.plot(x_axis, pdf_gaussian(x_axis, dx), label = 'dx')
    plt.plot(x_axis, pdf_gaussian(x_axis, prior), "-.", label = 'soma')
    plt.legend(["x", "dx", "Soma"])

def plot_gaussian(x_mean, x_var):
    plt.figure(figsize = (9, 6))
    x_axis = np.linspace(-10,10,100)
    x = gaussian(x_mean, x_var)
    pdf = pdf_gaussian(x_axis, x)
    plt.xlim(-9, 9)
    plt.plot(x_axis, pdf)
    plt.ylim(0, 0.6)
    plt.vlines(x=x_mean, ymin=0, ymax=max(pdf), color='g', zorder=2)
    plt.grid()

def plot_kalman(t, pos_real, z, predito, estimado, R):
    plt.xlim([0, len(pos_real)])
    plt.ylim([min(pos_real) - R*1.5, max(pos_real)+R*1.5])
    
    plt.plot(pos_real[0:t], '-.C4', alpha = 0.7, label = "Posicao real")
    plt.plot(z[0:t], 'xrC3', ms = 8, mew = 2, label = "Posicao medida (z)")
    plt.plot(predito[0:t], 'oC2', label = "Posicao predita (prior)")
    plt.plot(estimado[0:t], "C0", linewidth = 2.5, label = "Posicao estimada (posterior)")
    plt.grid()
    plt.legend(prop={'size': 22})
    
    
def interactive_covariance(v_a, v_b, v_ab):
    plt.figure(figsize = (9, 6))
    mean_a = 0
    mean_b = 0
    std = [1, 2, 3]
    P = [[v_a, v_ab], [v_ab, v_b]]
    plot_covariance((mean_a, mean_b), P, fc='g', alpha=0.2, 
                            std=std,
                            title= '|' + str(v_a) + ' ' + str(v_ab) + "|\n|" + str(v_ab) + ' ' + str(v_b) + "|")
    
    
def plot_interactive_covariance_multiply(x1, x2, x12, z1, z2, z12):
    plt.figure(figsize = (9, 6))
    P1 = [[x1, x12],[x12,x2]]
    P2 = [[z1,z12],[z12,z2]]
    P3 = multivariate_multiply((0, 0), P1, (0, 0), P2)[1]
    plot_covariance_ellipse((0, 0), P1, ec='k', fc='y', alpha=0.6)
    plot_covariance_ellipse((0, 0), P2, ec='k', fc='g', alpha=0.6)
    plot_covariance_ellipse((0, 0), P3, ec='k', fc='b')


def plot_kalman_steps(step, pos_real, modelo, predito, estimado, zs, R):
    step +=3
    n_steps = 4
    t = step//n_steps
    plt.figure(figsize = (21, 7))
    plt.subplot(1, 2, 1)
    plot_kalman(len(zs)-1, pos_real, zs, [p.mean for p in predito], [e.mean for e in estimado], R)
    plt.subplot(1, 2, 2)
    if(step % n_steps == 0):
        x_axis = np.linspace(0,max([e.mean for e in estimado]),200)
        pdf_pos = pdf_gaussian(x_axis, estimado[t-1])
        pdf_mod = pdf_gaussian(x_axis, modelo[t])
        pdf_prior = pdf_gaussian(x_axis, predito[t])
        plt.ylim(0, .5)
        plt.plot(x_axis, pdf_pos, "C0", label = "posterior(t-1)")
        plt.plot(x_axis, pdf_mod, "C1", label = "modelo")
        plt.plot(x_axis, pdf_prior, "--C2", label = "prior")
        plt.legend(prop={'size': 22})
        plt.title(f"Predict (step {t})", fontsize = 25)
    elif(step % n_steps == 1):
        x_axis = np.linspace(0,max([e.mean for e in estimado]),200)
        pdf_prior = pdf_gaussian(x_axis, predito[t])
        plt.ylim(0, .5)
        plt.plot(x_axis, pdf_prior, "C2", label = "prior")
        plt.legend(prop={'size': 22})
        plt.title(f"Predict (step {t})", fontsize = 25)
    elif(step % n_steps == 2):
        x_axis = np.linspace(0,max([e.mean for e in estimado]),200)
        pdf_z = pdf_gaussian(x_axis, gaussian(zs[t], R))
        pdf_prior = pdf_gaussian(x_axis, predito[t])
        pdf_pos = pdf_gaussian(x_axis, estimado[t])
        plt.ylim(0, .5)
        plt.plot(x_axis, pdf_prior, "C2", label = "prior")
        plt.plot(x_axis, pdf_z, "C3", label = "z")
        plt.plot(x_axis, pdf_pos, "--C0", label = "posterior")
        plt.legend(prop={'size': 22})
        plt.title(f"Update (step {t})", fontsize = 25)
    elif(step % n_steps == 3):
        x_axis = np.linspace(0,max([e.mean for e in estimado]),200)
        pdf_posterior = pdf_gaussian(x_axis, estimado[t])
        plt.ylim(0, .5)
        plt.plot(x_axis, pdf_posterior, "C0", label = "posterior")
        plt.legend(prop={'size': 22})
        plt.title(f"Update (step {t})", fontsize = 25)
    
    plt.tight_layout()

def cov_matrix(var_a, var_b, corr_ab):
    M_ab = (np.sqrt(var_a) * np.sqrt(var_b)) * corr_ab
    M = np.array([[var_a, M_ab], [M_ab, var_b]])
    return M

def plot_coelhinhos(t,teo, zs, pred, xs, dono, ax):
    #Plota os valor Estimados, Observações e Modeo
    
    ax.plot(t,teo, '-.C4', label = ('Teorico. ' + dono))
    ax.plot(t,zs, 'xC3', label = ('Observado (z). ' + dono))
    ax.plot(t,pred, '.C2', label = ('Predito (Prior). ' + dono))
    ax.plot(t,xs, 'C0', label = ('Estimado (Posterior). ' + dono))
    ax.set_xlabel('Tempo')
    ax.set_ylabel('Número de Coelhos')
    ax.grid()
    ax.legend()

def flatten(m):
    """Transforms matrix into 1D array"""
    return np.asarray(m.T).reshape(-1)

def plot_kf_results(gps, x_kf):
    plt.figure(figsize = (12,4), dpi = 100)
    plt.subplot(131)
    plt.plot(gps["x"], gps["y"], '', label = "Trajetória real")
    plt.plot(gps["x_obs"], gps["y_obs"], 'xC3', alpha = 0.5, label = "Observações")
    plt.plot(x_kf[:,0], x_kf[:,2], '--C2', label = "Estimativa Filtro de Kalman")
    plt.xlabel("$S_x$")
    plt.ylabel("$S_y$")
    plt.title("Trajetória GPS")
    
    plt.subplot(132)
    plt.plot(x_kf[:,1])
    plt.title("Velocidade $V_x$")
    
    plt.subplot(133)
    plt.plot(x_kf[:,1])
    plt.title("Velocidade $V_y$")