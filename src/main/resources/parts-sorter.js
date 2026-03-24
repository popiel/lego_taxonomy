(function() {
    const CATEGORY_COLUMNS = window.columnOrder.CATEGORY_COLUMNS;

    let currentOrder = [...CATEGORY_COLUMNS, 'image', 'color', 'quantity', 'name', 'partNumber'];
    let draggedColId = null;
    let draggedColType = null;
    let draggedElement = null;

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

    function handleDragStart(e) {
        draggedColId = e.target.dataset.colId;
        draggedColType = e.target.dataset.colType;
        draggedElement = e.target;
        e.target.classList.add('dragging');
        e.dataTransfer.effectAllowed = 'move';
    }

    function handleTouchStart(e) {
        const touch = e.touches[0];
        draggedElement = e.target.closest('th[draggable="true"]');
        if (draggedElement) {
            draggedColId = draggedElement.dataset.colId;
            draggedColType = draggedElement.dataset.colType;
            draggedElement.classList.add('dragging');
        }
    }

    function handleDragEnd(e) {
        e.target.classList.remove('dragging');
        document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
        removeDropIndicator();
        draggedColId = null;
        draggedColType = null;
        draggedElement = null;
    }

    function handleDragOver(e) {
        e.preventDefault();
        const th = e.target.closest('th[data-col-id]');
        if (!th || !canDropInPosition(th)) {
            removeDropIndicator();
            return;
        }
        e.dataTransfer.dropEffect = 'move';
        showDropIndicator(th, e.clientX);
    }

    function handleDragLeave(e) {
        const th = e.target.closest('th[data-col-id]');
        if (th) th.classList.remove('drag-over');
        removeDropIndicator();
    }

    function handleDrop(e) {
        e.preventDefault();
        const th = e.target.closest('th[data-col-id]');
        if (th) handleDropTarget(th);
    }

    function handleTouchMove(e) {
        if (!draggedElement) return;
        e.preventDefault();
        const touch = e.touches[0];
        const elementBelow = document.elementFromPoint(touch.clientX, touch.clientY);
        document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
        const th = elementBelow?.closest('th[data-col-id]');
        if (th && canDropInPosition(th)) {
            th.classList.add('drag-over');
            showDropIndicator(th, touch.clientX);
        } else {
            removeDropIndicator();
        }
    }

    function handleTouchEnd(e) {
        if (!draggedElement) return;
        e.preventDefault();
        const elementBelow = document.elementFromPoint(
            e.changedTouches[0].clientX,
            e.changedTouches[0].clientY
        );
        const th = elementBelow?.closest('th[data-col-id]');
        if (th) handleDropTarget(th);
        draggedElement.classList.remove('dragging');
        document.querySelectorAll('.drag-over').forEach(el => el.classList.remove('drag-over'));
        removeDropIndicator();
        draggedColId = null;
        draggedColType = null;
        draggedElement = null;
    }

    function showDropIndicator(th, clientX) {
        removeDropIndicator();
        const rect = th.getBoundingClientRect();
        const isAfter = clientX > rect.left + rect.width / 2;
        const indicator = document.createElement('div');
        indicator.className = 'drop-indicator ' + (isAfter ? 'after' : 'before');
        th.style.position = 'relative';
        th.appendChild(indicator);
    }

    function removeDropIndicator() {
        document.querySelectorAll('.drop-indicator').forEach(el => el.remove());
    }

    function canDropInPosition(th) {
        const targetIndex = currentOrder.indexOf(th.dataset.colId);
        const constrainedIndex = window.columnOrder.constrainDropTarget(
            currentOrder,
            targetIndex,
            draggedColType !== 'category'
        );
        return constrainedIndex === targetIndex;
    }

    function handleDropTarget(th) {
        th.classList.remove('drag-over');
        const targetColId = th.dataset.colId;
        const fromIndex = currentOrder.indexOf(draggedColId);
        let toIndex = currentOrder.indexOf(targetColId);
        toIndex = window.columnOrder.constrainDropTarget(currentOrder, toIndex, draggedColType !== 'category');
        if (fromIndex !== toIndex) {
            if (draggedColType === 'category') {
                currentOrder = window.columnOrder.moveCategoryGroup(currentOrder, fromIndex, toIndex);
            } else {
                currentOrder = window.columnOrder.moveColumn(currentOrder, fromIndex, toIndex);
            }
            reorderTable();
        }
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
