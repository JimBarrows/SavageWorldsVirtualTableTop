import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { BrowserRouter } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from 'react-query';
import SceneEditPage from './SceneEdit';
import sceneService from '../services/sceneService';

// Mock the services
jest.mock('../services/sceneService');

// Mock useNavigate and useParams
const mockNavigate = jest.fn();
const mockParams = { name: 'test-scene' };
jest.mock('react-router-dom', () => ({
  ...jest.requireActual('react-router-dom'),
  useNavigate: () => mockNavigate,
  useParams: () => mockParams
}));

// Mock SceneEditor component
jest.mock('../components/scene/SceneEditor', () => {
  return function MockSceneEditor({ scene, onSceneChange, onSave, onCancel, availableCharacters }) {
    return (
      <div data-testid="scene-editor">
        <div>Scene Editor Mock</div>
        <div>Scene: {scene?.name || 'Loading...'}</div>
        <div>Available Characters: {availableCharacters?.length || 0}</div>
        <button onClick={() => onSceneChange({ ...scene, name: 'Updated Scene' })}>
          Update Scene
        </button>
        <button onClick={() => onSave(scene)}>Save</button>
        <button onClick={onCancel}>Cancel</button>
      </div>
    );
  };
});

describe.skip('SceneEditPage', () => {
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

  const mockScene = {
    id: 'scene-123',
    name: 'test-scene',
    displayName: 'Test Scene',
    description: 'A test scene',
    characters: []
  };

  const renderComponent = () => {
    return render(
      <QueryClientProvider client={queryClient}>
        <BrowserRouter>
          <SceneEditPage />
        </BrowserRouter>
      </QueryClientProvider>
    );
  };

  describe('Initial Rendering', () => {
    it('should render loading state initially', () => {
      sceneService.getSceneByName.mockImplementation(() => new Promise(() => {}));
      renderComponent();
      expect(screen.getByText('Loading scene...')).toBeInTheDocument();
    });

    it('should render the page header when scene is loaded', async () => {
      sceneService.getSceneByName.mockResolvedValue(mockScene);
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Edit Scene')).toBeInTheDocument();
      });
    });

    it('should load and display the scene', async () => {
      sceneService.getSceneByName.mockResolvedValue(mockScene);
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene: Test Scene')).toBeInTheDocument();
        expect(sceneService.getSceneByName).toHaveBeenCalledWith('test-scene');
      });
    });

    it('should fetch and display available characters', async () => {
      sceneService.getSceneByName.mockResolvedValue(mockScene);
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Available Characters: 4')).toBeInTheDocument();
      });
    });
  });

  describe('Scene Loading Errors', () => {
    it('should handle scene loading error', async () => {
      sceneService.getSceneByName.mockRejectedValue(new Error('Failed to load'));
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene not found')).toBeInTheDocument();
      });
    });

    it('should show back to scenes button on error', async () => {
      sceneService.getSceneByName.mockRejectedValue(new Error('Failed to load'));
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Back to Scenes')).toBeInTheDocument();
      });
    });

    it('should navigate back when back button is clicked', async () => {
      sceneService.getSceneByName.mockRejectedValue(new Error('Failed to load'));
      
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene not found')).toBeInTheDocument();
      });

      fireEvent.click(screen.getByText('Back to Scenes'));

      expect(mockNavigate).toHaveBeenCalledWith('/scenes');
    });
  });

  describe('Scene Updates', () => {
    beforeEach(async () => {
      sceneService.getSceneByName.mockResolvedValue(mockScene);
    });

    it('should update scene successfully and navigate to scenes list', async () => {
      sceneService.updateScene.mockResolvedValue({
        ...mockScene,
        name: 'Updated Scene'
      });

      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene: test-scene')).toBeInTheDocument();
      });

      const saveButton = screen.getByText('Save');
      fireEvent.click(saveButton);

      await waitFor(() => {
        expect(sceneService.updateScene).toHaveBeenCalledWith(expect.any(Object));
        expect(mockNavigate).toHaveBeenCalledWith('/scenes');
      });
    });

    it('should handle scene update error', async () => {
      const consoleErrorSpy = jest.spyOn(console, 'error').mockImplementation();
      const alertSpy = jest.spyOn(window, 'alert').mockImplementation();
      
      sceneService.updateScene.mockRejectedValue(new Error('Update failed'));

      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene: test-scene')).toBeInTheDocument();
      });

      const saveButton = screen.getByText('Save');
      fireEvent.click(saveButton);

      await waitFor(() => {
        expect(consoleErrorSpy).toHaveBeenCalledWith(
          'Failed to update scene:',
          expect.any(Error)
        );
        expect(alertSpy).toHaveBeenCalledWith('Failed to update scene. Please try again.');
      });

      consoleErrorSpy.mockRestore();
      alertSpy.mockRestore();
    });

    it('should handle scene change updates', async () => {
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene: test-scene')).toBeInTheDocument();
      });

      const updateButton = screen.getByText('Update Scene');
      fireEvent.click(updateButton);

      // Scene state should be updated (internal state change)
      expect(screen.getByTestId('scene-editor')).toBeInTheDocument();
    });
  });

  describe('Navigation', () => {
    it('should handle cancel navigation', async () => {
      sceneService.getSceneByName.mockResolvedValue(mockScene);
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene: test-scene')).toBeInTheDocument();
      });

      const cancelButton = screen.getByText('Cancel');
      fireEvent.click(cancelButton);

      expect(mockNavigate).toHaveBeenCalledWith('/scenes');
    });
  });

  describe('Character Query', () => {
    beforeEach(() => {
      sceneService.getSceneByName.mockResolvedValue(mockScene);
    });

    it('should handle empty character list', async () => {
      queryClient.setQueryData(['characters'], []);
      
      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Available Characters: 0')).toBeInTheDocument();
      });
    });

    it('should handle character query error gracefully', async () => {
      queryClient.setQueryDefaults(['characters'], {
        queryFn: () => Promise.reject(new Error('Failed to fetch'))
      });

      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByTestId('scene-editor')).toBeInTheDocument();
      });
    });
  });

  describe('Mutation State', () => {
    beforeEach(() => {
      sceneService.getSceneByName.mockResolvedValue(mockScene);
    });

    it('should show loading state during update', async () => {
      let resolveUpdate;
      sceneService.updateScene.mockImplementation(() => 
        new Promise(resolve => { resolveUpdate = resolve; })
      );

      renderComponent();
      
      await waitFor(() => {
        expect(screen.getByText('Scene: test-scene')).toBeInTheDocument();
      });

      const saveButton = screen.getByText('Save');
      fireEvent.click(saveButton);

      // Update should be in progress
      expect(sceneService.updateScene).toHaveBeenCalled();

      // Resolve the update
      resolveUpdate({ ...mockScene, name: 'Updated' });

      await waitFor(() => {
        expect(mockNavigate).toHaveBeenCalledWith('/scenes');
      });
    });
  });
});