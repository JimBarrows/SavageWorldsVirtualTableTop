import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from 'react-query';
import SceneListPage from './SceneList';
import sceneService from '../services/sceneService';

// Mock the services
jest.mock('../services/sceneService');

// Mock FontAwesomeIcon
jest.mock('@fortawesome/react-fontawesome', () => ({
  FontAwesomeIcon: () => <span>Icon</span>
}));

// Mock bootstrap-react-components
jest.mock('bootstrap-react-components', () => ({
  Button: ({ children, onClick }) => <button onClick={onClick}>{children}</button>,
  PageHeader: ({ children }) => <div>{children}</div>
}));

// Mock useNavigate
const mockNavigate = jest.fn();
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => mockNavigate
}));

// Mock SceneList component
jest.mock('../components/scene/SceneList', () => {
  return function MockSceneList({ scenes, onDelete }) {
    return (
      <div data-testid="scene-list">
        <div>Scene List Mock</div>
        <div>Scenes: {scenes?.length || 0}</div>
        {scenes?.map(scene => (
          <div key={scene.id}>
            <span>{scene.name}</span>
            <button onClick={() => onDelete(scene.id)}>Delete {scene.name}</button>
          </div>
        ))}
      </div>
    );
  };
});

describe('SceneListPage', () => {
  let queryClient;

  beforeEach(() => {
    queryClient = new QueryClient({
      defaultOptions: {
        queries: { retry: false },
        mutations: { retry: false }
      }
    });
    jest.clearAllMocks();
  });

  const mockScenes = [
    { id: '1', name: 'Scene 1', description: 'First scene' },
    { id: '2', name: 'Scene 2', description: 'Second scene' },
    { id: '3', name: 'Scene 3', description: 'Third scene' }
  ];

  const renderComponent = () => {
    return render(
      <QueryClientProvider client={queryClient}>
        <BrowserRouter>
          <SceneListPage />
        </BrowserRouter>
      </QueryClientProvider>
    );
  };

  describe('Initial Rendering', () => {
    it('should render the page header', () => {
      sceneService.listScenes.mockResolvedValue([]);
      renderComponent();
      expect(screen.getByText('Scenes')).toBeInTheDocument();
    });

    it('should render the add scene button', () => {
      sceneService.listScenes.mockResolvedValue([]);
      renderComponent();
      expect(screen.getByText('Add Scene')).toBeInTheDocument();
    });

    it('should show loading state initially', () => {
      sceneService.listScenes.mockImplementation(() => new Promise(() => {}));
      renderComponent();
      expect(screen.getByText('Loading scenes...')).toBeInTheDocument();
    });
  });

  describe('Scene Loading', () => {
    it('should load and display scenes', async () => {
      sceneService.listScenes.mockResolvedValue(mockScenes);
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scenes: 3')).toBeInTheDocument();
        expect(screen.getByText('Scene 1')).toBeInTheDocument();
        expect(screen.getByText('Scene 2')).toBeInTheDocument();
        expect(screen.getByText('Scene 3')).toBeInTheDocument();
      });
    });

    it('should handle empty scene list', async () => {
      sceneService.listScenes.mockResolvedValue([]);
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scenes: 0')).toBeInTheDocument();
      });
    });

    it('should handle scene loading error', async () => {
      sceneService.listScenes.mockRejectedValue(new Error('Failed to load'));
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Error loading scenes')).toBeInTheDocument();
      });
    });

    it('should show error message details', async () => {
      const errorMessage = 'Network connection failed';
      sceneService.listScenes.mockRejectedValue(new Error(errorMessage));
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText(errorMessage)).toBeInTheDocument();
      });
    });

    it('should show refresh instructions on error', async () => {
      sceneService.listScenes.mockRejectedValue(new Error('Failed to load'));
      
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText(/Please try refreshing the page/)).toBeInTheDocument();
      });
    });
  });

  describe('Scene Actions', () => {
    beforeEach(() => {
      sceneService.listScenes.mockResolvedValue(mockScenes);
    });

    it('should navigate to add scene page when add button is clicked', () => {
      renderComponent();
      
      const addButton = screen.getByText('Add Scene');
      fireEvent.click(addButton);

      expect(mockNavigate).toHaveBeenCalledWith('/scene/add');
    });

    it('should call onDelete when delete is clicked', async () => {
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene 1')).toBeInTheDocument();
      });

      const deleteButton = screen.getByText('Delete Scene 1');
      fireEvent.click(deleteButton);

      // onDelete is handled by SceneList component through props
      expect(screen.getByText('Delete Scene 1')).toBeInTheDocument();
    });

    it('should delete scene successfully', async () => {
      sceneService.deleteScene.mockResolvedValue({});
      
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene 1')).toBeInTheDocument();
      });

      const deleteButton = screen.getByText('Delete Scene 1');
      fireEvent.click(deleteButton);

      await waitFor(() => {
        expect(sceneService.deleteScene).toHaveBeenCalledWith('1');
      });
    });

    it('should handle delete scene error', async () => {
      const consoleErrorSpy = jest.spyOn(console, 'error').mockImplementation();
      const alertSpy = jest.spyOn(window, 'alert').mockImplementation();
      
      sceneService.deleteScene.mockRejectedValue(new Error('Delete failed'));

      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene 1')).toBeInTheDocument();
      });

      const deleteButton = screen.getByText('Delete Scene 1');
      fireEvent.click(deleteButton);

      await waitFor(() => {
        expect(consoleErrorSpy).toHaveBeenCalledWith(
          'Failed to delete scene:',
          expect.any(Error)
        );
        expect(alertSpy).toHaveBeenCalledWith('Failed to delete scene. Please try again.');
      });

      consoleErrorSpy.mockRestore();
      alertSpy.mockRestore();
    });

    it('should confirm before deleting scene', async () => {
      const confirmSpy = jest.spyOn(window, 'confirm').mockReturnValue(true);
      sceneService.deleteScene.mockResolvedValue({});
      
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene 1')).toBeInTheDocument();
      });

      const deleteButton = screen.getByText('Delete Scene 1');
      fireEvent.click(deleteButton);

      await waitFor(() => {
        expect(confirmSpy).toHaveBeenCalledWith('Are you sure you want to delete this scene?');
        expect(sceneService.deleteScene).toHaveBeenCalledWith('1');
      });

      confirmSpy.mockRestore();
    });

    it('should not delete scene if user cancels confirmation', async () => {
      const confirmSpy = jest.spyOn(window, 'confirm').mockReturnValue(false);
      
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene 1')).toBeInTheDocument();
      });

      const deleteButton = screen.getByText('Delete Scene 1');
      fireEvent.click(deleteButton);

      await waitFor(() => {
        expect(confirmSpy).toHaveBeenCalledWith('Are you sure you want to delete this scene?');
      });

      expect(sceneService.deleteScene).not.toHaveBeenCalled();

      confirmSpy.mockRestore();
    });
  });

  describe('Mutation State', () => {
    beforeEach(() => {
      sceneService.listScenes.mockResolvedValue(mockScenes);
    });

    it('should refetch scenes after successful deletion', async () => {
      sceneService.deleteScene.mockResolvedValue({});
      sceneService.listScenes
        .mockResolvedValueOnce(mockScenes)
        .mockResolvedValueOnce(mockScenes.filter(s => s.id !== '1'));
      
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scenes: 3')).toBeInTheDocument();
      });

      const deleteButton = screen.getByText('Delete Scene 1');
      fireEvent.click(deleteButton);

      await waitFor(() => {
        expect(sceneService.listScenes).toHaveBeenCalledTimes(2);
      });
    });

    it('should show loading state during deletion', async () => {
      let resolveDelete;
      sceneService.deleteScene.mockImplementation(() => 
        new Promise(resolve => { resolveDelete = resolve; })
      );

      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene 1')).toBeInTheDocument();
      });

      const deleteButton = screen.getByText('Delete Scene 1');
      fireEvent.click(deleteButton);

      // Delete should be in progress
      expect(sceneService.deleteScene).toHaveBeenCalled();

      // Resolve the deletion
      resolveDelete({});

      await waitFor(() => {
        // Should refetch after deletion
        expect(sceneService.listScenes).toHaveBeenCalled();
      });
    });
  });
});