import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Link } from 'react-router-dom';
import './MarketingPage.css';

class MarketingPage extends Component {
  static propTypes = {
    className: PropTypes.string
  };

  render() {
    const { className = '' } = this.props;

    return (
      <div 
        className={`marketing-page container-fluid ${className}`} 
        data-testid="marketing-page"
        role="main"
      >
        {/* Hero Section */}
        <section className="hero-section text-center py-5">
          <div className="container">
            <div className="row">
              <div className="col-12">
                <img 
                  src="/assets/images/savage-worlds-logo.png" 
                  alt="Savage Worlds Virtual Table Top Logo"
                  className="logo mb-4"
                />
                <h1 className="display-4 mb-4">Savage Worlds Virtual Table Top</h1>
                <p className="lead mb-4">
                  The ultimate digital companion for your Savage Worlds tabletop adventures. 
                  Streamline your game management with powerful tools for GMs and players alike.
                </p>
                <div className="cta-buttons">
                  <Link 
                    to="/signup" 
                    className="btn btn-primary btn-lg me-3"
                    role="link"
                  >
                    Sign Up Now
                  </Link>
                  <Link 
                    to="/login" 
                    className="btn btn-outline-secondary btn-lg"
                    role="link"
                  >
                    Login
                  </Link>
                </div>
              </div>
            </div>
          </div>
        </section>

        {/* Features Section */}
        <section className="features-section py-5 bg-light">
          <div className="container">
            <div className="row">
              <div className="col-12 text-center mb-5">
                <h2>Powerful Features for Epic Adventures</h2>
                <p className="text-muted">Everything you need to run engaging Savage Worlds campaigns</p>
              </div>
            </div>
            <div className="row">
              <div className="col-md-6 col-lg-3 mb-4">
                <div className="feature-card text-center">
                  <div className="feature-icon mb-3">
                    <i className="fas fa-map-marked-alt fa-3x text-primary"></i>
                  </div>
                  <h4>Plot Point Management</h4>
                  <p>Create, organize, and track plot points for your campaigns with ease. Keep your stories flowing seamlessly.</p>
                </div>
              </div>
              <div className="col-md-6 col-lg-3 mb-4">
                <div className="feature-card text-center">
                  <div className="feature-icon mb-3">
                    <i className="fas fa-user-friends fa-3x text-primary"></i>
                  </div>
                  <h4>Character Creation</h4>
                  <p>Build detailed player characters with comprehensive attribute, skill, and equipment tracking.</p>
                </div>
              </div>
              <div className="col-md-6 col-lg-3 mb-4">
                <div className="feature-card text-center">
                  <div className="feature-icon mb-3">
                    <i className="fas fa-dragon fa-3x text-primary"></i>
                  </div>
                  <h4>Beast Management</h4>
                  <p>Manage NPCs, monsters, and creatures with detailed stat blocks and behavioral notes.</p>
                </div>
              </div>
              <div className="col-md-6 col-lg-3 mb-4">
                <div className="feature-card text-center">
                  <div className="feature-icon mb-3">
                    <i className="fas fa-shield-alt fa-3x text-primary"></i>
                  </div>
                  <h4>Equipment Tracking</h4>
                  <p>Comprehensive inventory management for weapons, armor, and gear across all characters.</p>
                </div>
              </div>
            </div>
          </div>
        </section>

        {/* Benefits Section */}
        <section className="benefits-section py-5">
          <div className="container">
            <div className="row align-items-center">
              <div className="col-lg-6 mb-4">
                <h2>Why Choose SWVTT?</h2>
                <ul className="list-unstyled benefits-list">
                  <li className="mb-3">
                    <i className="fas fa-check-circle text-success me-3"></i>
                    <strong>Fast & Intuitive:</strong> Spend more time playing, less time managing paperwork
                  </li>
                  <li className="mb-3">
                    <i className="fas fa-check-circle text-success me-3"></i>
                    <strong>Cloud-Based:</strong> Access your campaigns from anywhere, anytime
                  </li>
                  <li className="mb-3">
                    <i className="fas fa-check-circle text-success me-3"></i>
                    <strong>Savage Worlds Optimized:</strong> Built specifically for the Savage Worlds system
                  </li>
                  <li className="mb-3">
                    <i className="fas fa-check-circle text-success me-3"></i>
                    <strong>Free to Start:</strong> Begin your digital adventure at no cost
                  </li>
                </ul>
              </div>
              <div className="col-lg-6 mb-4">
                <img 
                  src="/assets/images/swvtt-screenshot.png" 
                  alt="SWVTT Application Screenshot" 
                  className="img-fluid rounded shadow"
                />
              </div>
            </div>
          </div>
        </section>

        {/* Call to Action Section */}
        <section className="final-cta-section py-5 bg-primary text-white text-center">
          <div className="container">
            <div className="row">
              <div className="col-12">
                <h2 className="mb-4">Ready to Enhance Your Savage Worlds Experience?</h2>
                <p className="lead mb-4">
                  Join thousands of Game Masters who are already using SWVTT to run better campaigns.
                </p>
                <Link 
                  to="/signup" 
                  className="btn btn-light btn-lg"
                  role="link"
                >
                  Get Started Today
                </Link>
              </div>
            </div>
          </div>
        </section>

        {/* Footer */}
        <footer className="marketing-footer py-4 bg-dark text-white text-center">
          <div className="container">
            <div className="row">
              <div className="col-12">
                <p className="mb-2">&copy; 2024 Savage Worlds Virtual Table Top. All rights reserved.</p>
                <p className="text-muted small">
                  Savage Worlds and all associated marks and logos are trademarks of Pinnacle Entertainment Group.
                </p>
              </div>
            </div>
          </div>
        </footer>
      </div>
    );
  }
}

export default MarketingPage;