(function() {
    const CATEGORY_COLUMNS = window.columnOrder.CATEGORY_COLUMNS;

    let currentOrder = [...CATEGORY_COLUMNS, 'image', 'color', 'quantity', 'name', 'partNumber'];
    let draggedColId = null;
    let draggedColType = null;
    let draggedElement = null;
    let draggedColumnIndex = null;

    let dropPositionRanges = null;
    let boundaryCoordinates = null;
    let currentDropPosition = null;

    document.addEventListener('DOMContentLoaded', init);

    function init() {
        initDesktopDragDrop();
        initTouchDragDrop();
        document.getElementById('resetColumnsBtn')?.addEventListener('click', resetColumnOrder);

        document.getElementById('inputFile').addEventListener('change', function() {
            if (this.files.length > 0) {
                document.getElementById('setNumber').value = '';
                document.getElementById('uploadForm').submit();
            }
        });
        document.getElementById('setNumber').addEventListener('keydown', function(e) {
            if (e.key === 'Enter' && this.value.trim() !== '') {
                document.getElementById('inputFile').value = '';
                document.getElementById('uploadForm').submit();
            }
        });
    }

    function initDesktopDragDrop() {
        document.querySelectorAll('th[draggable="true"]').forEach(th => {
            th.addEventListener('dragstart', handleDragStart);
            th.addEventListener('dragend', handleDragEnd);
            th.addEventListener('dragover', handleDragOver);
            th.addEventListener('dragleave', handleDragLeave);
            th.addEventListener('drop', handleDrop);
        });
    }

    function initTouchDragDrop() {
        document.querySelectorAll('th[draggable="true"]').forEach(th => {
            th.addEventListener('touchstart', handleTouchStart, { passive: false });
            th.addEventListener('touchmove', handleTouchMove, { passive: false });
            th.addEventListener('touchend', handleTouchEnd, { passive: false });
        });
    }

    function computeBoundaryCoordinates() {
        const ths = document.querySelectorAll('th[data-col-id]');
        const coords = [];
        ths.forEach(th => {
            const rect = th.getBoundingClientRect();
            coords.push(rect.left);
        });
        return coords;
    }

    function handleDragStart(e) {
        draggedColId = e.target.dataset.colId;
        draggedColType = e.target.dataset.colType;
        draggedElement = e.target;
        draggedColumnIndex = currentOrder.indexOf(draggedColId);
        e.target.classList.add('dragging');
        e.dataTransfer.effectAllowed = 'move';

        initializeDragState();
    }

    function handleTouchStart(e) {
        const touch = e.touches[0];
        draggedElement = e.target.closest('th[draggable="true"]');
        if (draggedElement) {
            draggedColId = draggedElement.dataset.colId;
            draggedColType = draggedElement.dataset.colType;
            draggedColumnIndex = currentOrder.indexOf(draggedColId);
            draggedElement.classList.add('dragging');

            initializeDragState();
        }
    }

    function initializeDragState() {
        boundaryCoordinates = computeBoundaryCoordinates();
        const isDraggingCategory = draggedColType === 'category';
        const validPositions = window.columnOrder.getValidDropPositions(
            currentOrder,
            draggedColumnIndex,
            isDraggingCategory
        );
        dropPositionRanges = window.columnOrder.computeDropPositionRanges(
            boundaryCoordinates,
            validPositions
        );
        currentDropPosition = draggedColumnIndex;
    }

    function handleDragEnd(e) {
        e.target.classList.remove('dragging');
        document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
        removeDropIndicator();

        if (currentDropPosition !== null) {
            applyDrop();
        }

        cleanupDragState();
    }

    function handleDragOver(e) {
        e.preventDefault();
        e.dataTransfer.dropEffect = 'move';

        const x = e.clientX;
        const newDropPosition = window.columnOrder.getDropPositionFromX(dropPositionRanges, x);

        if (newDropPosition !== currentDropPosition) {
            currentDropPosition = newDropPosition;
            showDropIndicator(x);
        }
    }

    function handleDragLeave(e) {
        const th = e.target.closest('th[data-col-id]');
        if (th) th.classList.remove('drag-over');
    }

    function handleDrop(e) {
        e.preventDefault();
        if (currentDropPosition !== null) {
            applyDrop();
        }
        cleanupDragState();
    }

    function handleTouchMove(e) {
        if (!draggedElement) return;
        e.preventDefault();
        const touch = e.touches[0];
        document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));

        const x = touch.clientX;
        const newDropPosition = window.columnOrder.getDropPositionFromX(dropPositionRanges, x);

        if (newDropPosition !== currentDropPosition) {
            currentDropPosition = newDropPosition;
            showDropIndicator(x);
        }
    }

    function handleTouchEnd(e) {
        if (!draggedElement) return;
        e.preventDefault();

        if (currentDropPosition !== null) {
            applyDrop();
        }

        draggedElement.classList.remove('dragging');
        document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
        removeDropIndicator();

        cleanupDragState();
    }

    function showDropIndicator(x) {
        removeDropIndicator();

        const isDraggingCategory = draggedColType === 'category';
        const displayInfo = window.columnOrder.getDropPositionDisplayInfo(
            currentOrder,
            boundaryCoordinates,
            draggedColumnIndex,
            isDraggingCategory,
            currentDropPosition
        );

        if (displayInfo.type === 'box') {
            const indicator = document.createElement('div');
            indicator.className = 'drop-indicator-box';
            indicator.style.left = displayInfo.startBoundary + 'px';
            indicator.style.width = (displayInfo.endBoundary - displayInfo.startBoundary) + 'px';
            document.body.appendChild(indicator);
        } else {
            const indicator = document.createElement('div');
            indicator.className = 'drop-indicator-line';
            let leftPos;
            if (displayInfo.isEndOfTable) {
                const n = boundaryCoordinates.length;
                leftPos = boundaryCoordinates[n - 1] - 4;
            } else if (displayInfo.boundaryIndex === 0) {
                leftPos = boundaryCoordinates[0];
            } else {
                leftPos = boundaryCoordinates[displayInfo.boundaryIndex];
            }
            indicator.style.left = leftPos + 'px';
            document.body.appendChild(indicator);
        }
    }

    function removeDropIndicator() {
        document.querySelectorAll('.drop-indicator-box, .drop-indicator-line').forEach(el => el.remove());
    }

    function applyDrop() {
        if (currentDropPosition === null) return;

        const isDraggingCategory = draggedColType === 'category';
        const newOrder = window.columnOrder.moveToDropPosition(
            currentOrder,
            draggedColumnIndex,
            isDraggingCategory,
            currentDropPosition
        );

        if (newOrder.join(',') !== currentOrder.join(',')) {
            currentOrder = newOrder;
            reorderTable();
        }
    }

    function cleanupDragState() {
        dropPositionRanges = null;
        boundaryCoordinates = null;
        currentDropPosition = null;
        draggedColId = null;
        draggedColType = null;
        draggedElement = null;
        draggedColumnIndex = null;
    }

    function reorderTable() {
        const table = document.querySelector('table');
        if (!table) return;

        const headerCells = Array.from(table.querySelectorAll('thead th'));
        headerCells.sort((a, b) =>
            currentOrder.indexOf(a.dataset.colId) - currentOrder.indexOf(b.dataset.colId)
        );
        const headerRow = table.querySelector('thead tr');
        headerCells.forEach(cell => headerRow.appendChild(cell));

        table.querySelectorAll('tbody tr').forEach(row => {
            const cells = Array.from(row.querySelectorAll('td'));
            cells.sort((a, b) =>
                currentOrder.indexOf(a.dataset.colId) - currentOrder.indexOf(b.dataset.colId)
            );
            cells.forEach(cell => row.appendChild(cell));
        });
    }

    function resetColumnOrder() {
        currentOrder = window.columnOrder.resetColumns();
        reorderTable();
    }

    window.resetColumnOrder = resetColumnOrder;
})();
