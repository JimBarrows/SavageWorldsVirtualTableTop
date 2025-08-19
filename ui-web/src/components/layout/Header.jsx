import Navbar from 'bootstrap-react-components/distribution/bootstrap/components/Navbar';
import Brand from 'bootstrap-react-components/distribution/bootstrap/components/Navbar/Brand';
import PropTypes from 'prop-types';
import React from 'react';
import { useNavigate } from 'react-router-dom';
import { useAuth } from '../../contexts/AuthContext';

export default function Header({ id }) {
  const navigate = useNavigate();
  const { user, logout } = useAuth();

  const brandClicked = () => {
    navigate('/');
  };

  // Check for unsaved changes by looking for forms with unsaved state
  const checkForUnsavedChanges = (documentRef = document) => {
    // Check if there are any forms with unsaved changes
    // This is a simple implementation - in a more complex app you'd use a context
    const forms = documentRef.querySelectorAll('form');
    for (let form of forms) {
      // Look for any form elements that have been modified
      const inputs = form.querySelectorAll('input, textarea, select');
      for (let input of inputs) {
        if (input.defaultValue !== input.value) {
          return true;
        }
      }
    }
    return false;
  };

  const handleLogout = async () => {
    try {
      // Check for unsaved changes before logout
      const hasUnsavedChanges = checkForUnsavedChanges();
      
      if (hasUnsavedChanges) {
        const confirmed = window.confirm('You have unsaved changes. Are you sure you want to logout?');
        if (!confirmed) {
          return; // User cancelled logout
        }
      }

      await logout();
      // Show success message (will be cleared when component unmounts)
      // Navigate to login page with success message in state
      navigate('/login', { 
        state: { 
          message: 'You have been logged out successfully',
          type: 'success'
        }
      });
    } catch (error) {
      console.error('Logout error in header:', error);
      // Still navigate to login even if logout fails
      navigate('/login');
    }
  };

  return (
    <Navbar id={`header-${id}`}>
      <Brand id={`header-${id}`} onClick={brandClicked}>
        Savage Worlds
      </Brand>
      
      <div className="navbar-nav ms-auto">
        {user && (
          <>
            <span className="navbar-text me-3">
              Welcome, {user.email || user.username}
            </span>
            <button 
              className="btn btn-outline-secondary btn-sm" 
              onClick={handleLogout}
              data-testid="logout-button"
              data-test="logout"
            >
              Logout
            </button>
          </>
        )}
      </div>
    </Navbar>
  );
}

Header.propTypes = {
  id: PropTypes.string.isRequired,
};