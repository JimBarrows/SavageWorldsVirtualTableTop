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

  const handleLogout = async () => {
    await logout();
    navigate('/login');
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